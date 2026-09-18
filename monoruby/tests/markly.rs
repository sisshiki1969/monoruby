extern crate monoruby;
use monoruby::tests::*;

// The markly gem over monoruby's stand-in for markly.so
// (gem/markly/markly.rb + src/builtins/markly.rs on the comrak crate).
// Every case is compared against the host CRuby, which runs the gem's real
// C extension over cmark-gfm — so these pin the stand-in to what markly
// actually does: the HTML it renders, the tree it exposes, the errors it
// raises.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and markly is an ordinary gem.
//
// `to_commonmark` is deliberately not compared: comrak re-serializes with
// its own spacing (`- ` vs `  - `, `1. ` vs `1.  `, `\` vs two spaces for
// a hard break), and the gem only uses it to `dup` a node, which is
// covered through the re-parse.

/// The GFM document Lobsters' Markdowner renders: smart punctuation,
/// emphasis, strikethrough, code, raw HTML with the tag filter, lists,
/// quotes, fenced and indented code, a table, task items, autolinks and
/// both line-break kinds — as HTML, with and without `UNSAFE`.
#[test]
fn markly_render_html() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        src = <<~MD
          # Title

          Hello @user *em* **strong** ~~del~~ `code` <b>b</b> <script>x</script> "quotes" -- dash...

          - one
          - two [link](http://example.com "t")

          1. a
          2. b

          > quote
          > more

          ```ruby
          puts 1
          ```

              indented

          | a | b |
          |:--|--:|
          | 1 | 2 |

          - [ ] todo
          - [x] done

          http://auto.link/x and www.example.com

          ---
          Line one
          Line two  
          hard
        MD
        exts = [:tagfilter, :autolink, :strikethrough, :table, :tasklist]
        root = Markly.parse(src, flags: Markly::SMART, extensions: exts)
        res = []
        res << root.to_html(flags: Markly::DEFAULT, extensions: exts)
        res << root.to_html(flags: Markly::UNSAFE, extensions: exts)
        res << root.to_html(flags: Markly::UNSAFE, extensions: [])
        res << root.to_html.encoding.name
        res << Markly.render_html(src, extensions: exts)
        res << Markly.render_html("a\nb", flags: Markly::HARD_BREAKS)
        res << Markly.render_html("<div>x</div>\n\n<b>y</b>")
        res << Markly.render_html("<div>x</div>\n\n<b>y</b>", flags: Markly::UNSAFE)
        res << Markly.render_html("[x](javascript:alert(1)) ![i](http://x/y.png \"t\")")
        res << Markly.render_html("# h\n\npara", flags: Markly::SOURCE_POSITION)
        res << Markly.render_html("```rb x\ncode\n```", flags: Markly::GITHUB_PRE_LANG)
        res << Markly.render_html("```rb x\ncode\n```", flags: Markly::FULL_INFO_STRING)
        res << Markly.render_html("hi[^1]\n\n[^1]: note", flags: Markly::FOOTNOTES)
        res << Markly.render_html("a ~b~ c", extensions: [:strikethrough])
        res << Markly.render_html("\"x\" -- 'y'...", flags: Markly::SMART)
        res << Markly.render_html("é ü 日本語 😀 &amp; &copy; \\*x\\*")
        res << Markly.render_html("")
        res
        "##,
    );
}

/// The tree: node types in walk order, `each`, type strings, source
/// positions, the per-kind attributes (`string_content`, `url`, `title`,
/// `header_level`, `list_*`, `fence_info`, `table_alignments`,
/// `tasklist_item_checked?`) and the `Markly::Error` the others raise.
#[test]
fn markly_tree_and_attributes() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        src = "# Title\n\npara *em* [l](http://x \"t\") ![i](y)\n\n3) a\n4) b\n\n- x\n\n```rb\ncode\n```\n\n| a | b |\n|:-:|--:|\n| 1 | 2 |\n\n- [x] done\n\n<div>h</div>\n\ntext <i>ih</i> `c`\n"
        root = Markly.parse(src, extensions: [:table, :tasklist])
        res = []
        res << root.walk.map(&:type)
        res << root.walk.map(&:type_string)
        res << root.each.map(&:type)
        # Block end positions differ between cmark-gfm and comrak for lists;
        # compare the positions of the inline-bearing nodes.
        res << Markly.parse("# T\n\npara *em* [l](http://x)\ntwo\n\n> q\n\n```\nc\n```\n").walk.map { |n| [n.type, n.source_position] }
        res << root.walk.map { |n| n.string_content }
        res << root.walk.map { |n| n.string_content&.encoding&.name }.compact.uniq
        h = root.first_child
        res << [h.type, h.header_level, h.first_child.string_content, h.parent.type, h.next.type, h.previous, root.parent, root.next]
        link = root.walk.find { |n| n.type == :link }
        img = root.walk.find { |n| n.type == :image }
        res << [link.url, link.title, img.url, img.title]
        ol = root.walk.find { |n| n.type == :list && n.list_type == :ordered_list }
        ul = root.walk.find { |n| n.type == :list && n.list_type == :bullet_list }
        res << [ol.list_type, ol.list_start, ol.list_tight, ul.list_type, ul.list_tight]
        cb = root.walk.find { |n| n.type == :code_block }
        res << [cb.fence_info, cb.string_content]
        tb = root.walk.find { |n| n.type == :table }
        res << [tb.table_alignments, tb.walk.map(&:type)]
        task = root.walk.select { |n| n.type == :list_item }.map(&:tasklist_item_checked?)
        res << task
        res << [root.last_child.type, root.last_child.last_child.string_content]
        probe = lambda { |&b| begin; b.call; rescue => e; [e.class.name, e.message]; end }
        para = root.walk.find { |n| n.type == :paragraph }
        res << probe.call { para.url }
        res << probe.call { para.title }
        res << probe.call { para.header_level }
        res << probe.call { para.list_type }
        res << probe.call { para.fence_info }
        res << probe.call { para.table_alignments }
        res << probe.call { ul.list_start }
        res << probe.call { para.list_tight }
        res << probe.call { para.string_content = "x" }
        res << probe.call { para.url = "x" }
        res << probe.call { h.header_level = 9 }
        res << probe.call { ol.list_type = :bogus }
        res << probe.call { Markly::Node.new(:bogus) }
        res << probe.call { Markly.parse("x", extensions: [:nope]) }
        res << probe.call { root.to_html(extensions: ["table"]) }
        res << probe.call { root.to_html(extensions: [:nope]) }
        res << probe.call { Markly::Parser.new("x") }
        res << Markly.extensions
        res << [Markly::DEFAULT, Markly::SMART, Markly::UNSAFE, Markly::HARD_BREAKS, Markly::FOOTNOTES]
        res << root.walk.find { |n| n.type == :footnote_definition }
        fn_root = Markly.parse("hi[^a]\n\n[^a]: note *x*", flags: Markly::FOOTNOTES)
        res << fn_root.walk.map(&:type)
        res << fn_root.walk.map { |n| n.parent_footnote_def&.type }
        res << fn_root.to_html
        res
        "##,
    );
}

/// Editing the tree the way Lobsters' Markdowner does (splitting a text
/// node around a mention and inserting a link), plus every other
/// operation the gem builds on: `insert_before`, `prepend_child`,
/// `append_child`, `delete`, `replace`, `append_after` / `append_before`,
/// `extract_children`, `replace_section`, `dup`, and the setters.
#[test]
fn markly_tree_edits() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        exts = [:tagfilter, :autolink, :strikethrough]
        root = Markly.parse("Hello @user and @other *x*", flags: Markly::SMART, extensions: exts)
        root.walk do |node|
          next unless node.type == :text
          while (m = node.string_content.match(/@(\w+)/))
            before, after = m.pre_match, m.post_match
            node.string_content = before
            link = Markly::Node.new(:link)
            link.url = "/u/#{m[1]}"
            text = Markly::Node.new(:text)
            text.string_content = "@#{m[1]}"
            link.append_child(text)
            node.insert_after(link)
            rest = Markly::Node.new(:text)
            rest.string_content = after
            link.insert_after(rest)
            node = rest
          end
        end
        res = []
        res << root.to_html(flags: Markly::DEFAULT, extensions: exts)
        res << root.walk.map { |n| [n.type, (n.string_content rescue nil)] }

        doc = Markly.parse("# H1\n\np1\n\n## H2\n\np2\n\n# H3\n\np3")
        h2 = doc.find_header("H2")
        res << [h2&.type, h2&.header_level]
        para = Markly::Node.new(:paragraph)
        t = Markly::Node.new(:text)
        t.string_content = "new"
        para.append_child(t)
        h2.replace_section(para)
        res << doc.to_html
        res << doc.walk.map(&:type)

        doc = Markly.parse("a\n\nb\n\nc")
        b = doc.first_child.next
        n = Markly::Node.new(:paragraph)
        n.append_child(Markly::Node.new(:text).tap { |x| x.string_content = "before" })
        b.insert_before(n)
        n2 = Markly::Node.new(:hrule)
        b.insert_after(n2)
        res << doc.to_html
        res << [n.parent.type, n.next.type, n.previous.type, n2.previous.type, n2.next.type]
        b.delete
        res << [b.parent, b.next, b.previous, doc.to_html]
        n2.replace(Markly::Node.new(:paragraph).tap { |p| p.append_child(Markly::Node.new(:text).tap { |x| x.string_content = "repl" }) })
        res << doc.to_html
        res << doc.each.map(&:type)
        first = doc.first_child
        first.prepend_child(Markly::Node.new(:strong).tap { |s| s.append_child(Markly::Node.new(:text).tap { |x| x.string_content = "S" }) })
        first.append_child(Markly::Node.new(:code).tap { |c| c.string_content = "C" })
        res << doc.to_html
        res << [first.first_child.type, first.last_child.type]

        frag = Markly.parse("x *y* z").first_child.extract_children
        res << [frag.type, frag.each.map(&:type), frag.to_html]
        probe = lambda { |&b| begin; b.call; rescue => e; [e.class.name, e.message]; end }
        # A custom_inline fragment takes no block children (cmark's can_contain).
        res << probe.call { Markly.parse("p1\n\np2").extract_children }
        res << probe.call { Markly.parse("x").first_child.append_child(Markly::Node.new(:paragraph)) }
        res << probe.call { Markly.parse("x").append_child(Markly::Node.new(:text)) }
        res << probe.call { Markly.parse("x").first_child.insert_after(Markly::Node.new(:text)) }
        res << probe.call { Markly.parse("x").first_child.insert_after(Markly.parse("y")) }
        res << probe.call { d = Markly.parse("x"); d.first_child.append_child(d) }
        res << probe.call { d = Markly.parse("- a"); d.first_child.append_child(Markly::Node.new(:paragraph)) }
        res << probe.call { d = Markly.parse("x"); d.first_child.replace(Markly::Node.new(:text)) }
        res << probe.call { d = Markly.parse("x"); d.replace(Markly::Node.new(:paragraph)) }
        res << probe.call { d = Markly.parse("x"); d.first_child.prepend_child("str") }
        blocks = Markly.parse("p1\n\np2")
        frag = Markly::Node.new(:custom_block)
        frag.append_child(blocks.first_child)
        frag.append_child(blocks.first_child)
        res << [blocks.each.map(&:type), frag.each.map(&:type), frag.to_html]
        target = Markly.parse("t")
        target.first_child.append_after(Markly.parse("d1\n\nd2"))
        target.first_child.append_before(Markly.parse("b1"))
        res << target.to_html

        d = Markly.parse("# H\n\n- a\n- b").dup
        res << [d.type, d.to_html]

        h = Markly.parse("# x").first_child
        h.header_level = 3
        l = Markly.parse("- a").first_child
        l.list_type = :ordered_list
        l.list_start = 7
        l.list_tight = false
        cb = Markly.parse("```\nc\n```").first_child
        cb.fence_info = "ruby"
        cb.string_content = "puts 2\n"
        lk = Markly.parse("[a](b)").first_child.first_child
        lk.url = "http://z/?a=1&b=<2>"
        lk.title = "ti\"tle"
        ti = Markly.parse("- [ ] a", extensions: [:tasklist]).first_child.first_child
        ti.tasklist_item_checked = true
        res << [h, l, cb, lk.parent, ti.parent].map(&:to_html)
        res << [h.header_level, l.list_type, l.list_start, l.list_tight, cb.fence_info, lk.url, lk.title, ti.tasklist_item_checked?]
        res << [Markly.parse("x").first_child.html_escape_html("<a & \"b\">"), Markly.parse("x").first_child.html_escape_href("a b/é?x=<1>&y")]
        res
        "##,
    );
}

/// `to_plaintext`: cmark-gfm's plaintext renderer (block spacing, list
/// markers, the tilde the strikethrough extension keeps, the table's
/// alignment row, task markers, footnotes) with the default width of 0.
#[test]
fn markly_plaintext() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        src = <<~MD
          # Title

          Hello *em* **strong** ~~del~~ `code` <b>b</b> "q"
          soft
          hard  
          break

          - one
          - two [link](http://example.com "t")
            - nested

          1. a
          2. b

          > quote
          > more

          ```ruby
          puts 1
          ```

              indented

          | a | b | c |
          |:--|--:|:-:|
          | 1 | 2 | 3 |

          - [ ] todo
          - [x] done

          ---
          hi[^1]

          [^1]: note
        MD
        exts = [:tagfilter, :autolink, :strikethrough, :table, :tasklist]
        root = Markly.parse(src, flags: Markly::SMART | Markly::FOOTNOTES, extensions: exts)
        res = []
        res << root.to_plaintext
        res << root.to_plaintext(flags: Markly::HARD_BREAKS)
        res << root.to_plaintext.encoding.name
        res << Markly.parse("a\nb").to_plaintext(flags: Markly::NO_BREAKS)
        res << Markly.parse("- a\n- b\n\n\n    code\n- c").to_plaintext
        res << Markly.parse("1. a\n\n   b\n2. c\n10. d").to_plaintext
        res << Markly.parse("- a\n\n  b\n- c").first_child.to_plaintext
        res << Markly.parse("# h\n\np").first_child.to_plaintext
        res << Markly::Renderer::HTML.anchor_for(Markly.parse("# Hello  World").first_child)
        res
        "##,
    );
}

/// The gem's Ruby-side HTML renderer (`Markly::Renderer::HTML`) walks the
/// same tree and calls back into the node's escapers.
#[test]
fn markly_ruby_renderer() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        src = "# T\n\npara *em* **s** `c` [l](http://x \"t\") ![i](y \"z\")<br>\n\n- a\n- b\n\n1. c\n\n> q\n\n```rb\ncode\n```\n\n| a | b |\n|:-:|--:|\n| 1 | 2 |\n\n- [x] d\n\n---\nhard  \nbreak\n\n<div>h</div>\n\n~~s~~ x[^1]\n\n[^1]: note\n"
        exts = [:table, :tasklist, :strikethrough, :tagfilter]
        root = Markly.parse(src, flags: Markly::FOOTNOTES, extensions: exts)
        res = []
        res << Markly::Renderer::HTML.new(extensions: exts).render(root)
        res << Markly::Renderer::HTML.new(flags: Markly::UNSAFE, extensions: exts).render(root)
        # (list end positions differ between cmark-gfm and comrak, so the
        # source-position variant renders a list-free document)
        res << Markly::Renderer::HTML.new(flags: Markly::SOURCE_POSITION).render(Markly.parse("# T\n\npara *em*\n\n> q\n\n```\nc\n```\n\n---\n"))
        res << Markly::Renderer::HTML.new(ids: true).render(Markly.parse("# One\n\ntext\n\n## Two\n\nmore"))
        res
        "##,
    );
}

/// markly 0.19's additions — `_dup` (a detached deep copy behind `dup`),
/// `code_info` / `code_info=` / `code_language`, `fence`, the
/// `:front_matter` node — guarded by the installed gem's version so the
/// comparison holds on a host with 0.15 as well.
#[test]
fn markly_0_19_api() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        res = []
        if Gem::Version.new(Markly::VERSION) >= Gem::Version.new("0.19.0")
          doc = Markly.parse("# H\n\n- a\n- *b* `c`\n\n```ruby x\ncode\n```\n\n    indented\n")
          item = doc.first_child.next.first_child.dup
          res << [item.type, item.parent, item.each.map(&:type), item.to_html]
          copy = doc.dup
          copy.first_child.first_child.string_content = "changed"
          res << [doc.first_child.first_child.string_content, copy.first_child.first_child.string_content, copy.parent]
          cb = doc.walk.find { |n| n.type == :code_block }
          ind = doc.walk.select { |n| n.type == :code_block }.last
          res << [cb.code_info, cb.code_language, cb.fence.to_a, cb.fence.class.name, ind.code_info, ind.code_language, ind.fence]
          cb.code_info = "js"
          res << [cb.code_info, cb.fence_info, cb.to_html]
          cb.code_info = nil
          res << [cb.code_info, cb.to_html]
          code = doc.walk.find { |n| n.type == :code }
          res << [code.code_info, code.code_language, code.to_html]
          probe = lambda { |&b| begin; b.call; rescue => e; [e.class.name, e.message]; end }
          res << probe.call { doc.first_child.code_info }
          res << probe.call { doc.first_child.code_info = "x" }
          res << probe.call { cb.code_info = 1 }
          res << Markly::Node.new(:code_block).fence
          res << Markly::Node.new(:code_block).code_info
          res << Markly.parse("~~~~ py\nx\n~~~~").first_child.fence.to_a
          res << Markly::Renderer::HTML.new.render(doc)
          fm = Markly.parse("---\ntitle: x\n---\n\n# H\n", flags: Markly::FRONT_MATTER)
          res << fm.walk.map(&:type)
          res << [fm.first_child.type_string, fm.first_child.string_content, fm.first_child.code_info, fm.to_html]
          res << Markly.parse("---\ntitle: x\n---\n\n# H\n").walk.map(&:type)
        else
          res << :skipped
        end
        res
        "##,
    );
}

/// A code block's attributes come out in cmark-gfm's order, and the same
/// input renders the same way every time. comrak collects those
/// attributes in a `HashMap` and writes them in its iteration order, so
/// before `src/builtins/markly.rs` took the tag writing over through
/// comrak's `codefence_syntax_highlighter` hook, a tag carrying two of
/// them came out in an order that followed the hash seed: `<code
/// class="language-rb" data-meta="x">` on one run and `<code
/// data-meta="x" class="language-rb">` on the next. Each combination is
/// rendered many times in the one process, since a fresh `HashMap` is
/// what varies, and every rendering must agree with the gem's.
#[test]
fn markly_code_block_attribute_order() {
    run_test_once(
        r##"
        require "rubygems"
        require "markly"
        src = "```rb x\ncode\n```\n"
        flags = {
          plain: 0,
          full_info: Markly::FULL_INFO_STRING,
          pre_lang: Markly::GITHUB_PRE_LANG,
          pre_lang_full_info: Markly::GITHUB_PRE_LANG | Markly::FULL_INFO_STRING,
          source_position: Markly::SOURCE_POSITION,
          source_position_full_info: Markly::SOURCE_POSITION | Markly::FULL_INFO_STRING,
          source_position_pre_lang: Markly::SOURCE_POSITION | Markly::GITHUB_PRE_LANG,
          everything: Markly::SOURCE_POSITION | Markly::GITHUB_PRE_LANG | Markly::FULL_INFO_STRING,
        }
        res = []
        flags.each do |name, f|
          renderings = 40.times.map { Markly.render_html(src, flags: f) }.uniq
          res << [name, renderings.size, renderings.first.scan(/<(?:pre|code)[^>]*>/)]
        end
        res
        "##,
    );
}
