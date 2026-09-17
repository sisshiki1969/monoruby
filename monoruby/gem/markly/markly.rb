# markly/markly.rb – monoruby's stand-in for markly.so (the markly gem)
#
# The markly gem is `lib/markly.rb` (`Markly.parse` / `render_html`),
# `markly/node.rb` (`Node#each` / `walk` / `to_html` / `append_after` …),
# `markly/flags.rb` and the Ruby `Renderer` classes over a C extension that
# binds cmark-gfm: `Markly::Parser`, `Markly::Node`, `Markly::Error` and
# `Markly.extensions`. This file defines those in Ruby.
#
# The node tree lives here, as `Markly::Node` objects linked the way
# cmark's are (parent / first_child / last_child / next / previous), so the
# gem's in-place editing (`insert_after`, `delete`, `replace`, …) is plain
# pointer surgery. Parsing and rendering are native
# (`src/builtins/markly.rs`, on the comrak crate): `String.__markly_parse`
# answers the document as a nested Array, one row per node, and each
# `_render_*` hands the same shape back. The row layout is documented at
# the top of that file.
#
# `markly/flags.rb` is loaded after this file, so the flag constants are
# looked up lazily here.
#
# Both gem lines are served: markly 0.15.x (`dup` re-parses `to_markdown`)
# and 0.19.x (`_dup`, `code_info`, `fence`, the `:front_matter` node and
# the `FRONT_MATTER` flag). `INLINE_CODE_INFO` and `HTML_BLOCK_BLANK_LINES`
# (0.19) have no comrak counterpart and are ignored.

module Markly
  class Error < StandardError; end

  EXTENSION_NAMES = %w[table strikethrough autolink tagfilter tasklist].freeze
  private_constant :EXTENSION_NAMES

  # Public: the names of the syntax extensions cmark-gfm registers.
  def self.extensions
    EXTENSION_NAMES.dup
  end

  class Parser
    def initialize(flags)
      unless flags.is_a?(Integer)
        raise TypeError, "wrong argument type #{flags.class} (expected Integer)"
      end
      @flags = flags
      @extensions = []
    end

    def enable(extension)
      unless extension.is_a?(Symbol)
        raise TypeError, "wrong argument type #{extension.class} (expected Symbol)"
      end
      unless EXTENSION_NAMES.include?(extension.to_s)
        raise ArgumentError, "extension #{extension} not found"
      end
      @extensions << extension unless @extensions.include?(extension)
      nil
    end

    def parse(text)
      text = text.to_str unless text.is_a?(String)
      Node.__from_row(String.__markly_parse(text, @flags, @extensions), nil)
    end
  end

  class Node
    # The node types `Node.new` accepts (markly.c `rb_node_new`).
    NODE_TYPES = %i[
      document blockquote list list_item code_block html paragraph header
      hrule text softbreak linebreak code inline_html emph strong link image
      footnote_reference footnote_definition custom_block custom_inline
    ].freeze
    private_constant :NODE_TYPES

    # Slots of the native row.
    T_TYPE = 0
    T_CONTENT = 1
    T_URL = 2
    T_TITLE = 3
    T_LEVEL = 4
    T_LIST_TYPE = 5
    T_LIST_START = 6
    T_LIST_TIGHT = 7
    T_LIST_DELIM = 8
    T_FENCE_INFO = 9
    T_FENCED = 10
    T_ALIGNMENTS = 11
    T_TABLE_HEADER = 12
    T_CHECKED = 13
    T_EXTRA = 14
    T_SOURCEPOS = 15
    T_CHILDREN = 16
    private_constant :T_TYPE, :T_CONTENT, :T_URL, :T_TITLE, :T_LEVEL,
      :T_LIST_TYPE, :T_LIST_START, :T_LIST_TIGHT, :T_LIST_DELIM,
      :T_FENCE_INFO, :T_FENCED, :T_ALIGNMENTS, :T_TABLE_HEADER, :T_CHECKED,
      :T_EXTRA, :T_SOURCEPOS, :T_CHILDREN

    # Types whose literal `string_content` reads and writes
    # (cmark_node_get_literal / set_literal).
    LITERAL_TYPES = %i[code_block html text inline_html code footnote_reference footnote_definition front_matter].freeze
    private_constant :LITERAL_TYPES

    # cmark_node_get_type_string for the gem's symbols.
    TYPE_STRINGS = {
      document: "document", blockquote: "block_quote", list: "list",
      list_item: "item", code_block: "code_block", html: "html_block",
      paragraph: "paragraph", header: "heading", hrule: "thematic_break",
      text: "text", softbreak: "softbreak", linebreak: "linebreak",
      code: "code", inline_html: "html_inline", emph: "emph",
      strong: "strong", link: "link", image: "image",
      footnote_reference: "footnote_reference",
      footnote_definition: "footnote_definition",
      custom_block: "custom_block", custom_inline: "custom_inline",
      front_matter: "front_matter",
    }.freeze
    private_constant :TYPE_STRINGS

    # Public: Creates a detached node of the given type.
    def self.new(type)
      unless NODE_TYPES.include?(type)
        # The extension formats the (unset) numeric node type.
        raise Error, "invalid node of type 0"
      end
      node = allocate
      node.__send__(:__init, type)
      node
    end

    def self.__from_row(row, parent)
      node = allocate
      node.__send__(:__init_row, row, parent)
      node
    end

    attr_reader :type, :parent, :first_child, :last_child, :next, :previous

    def __init(type)
      @type = type
      @parent = @first_child = @last_child = @next = @previous = nil
      @content = nil
      @url = @title = nil
      @level = 0
      @list_type = :bullet_list
      @list_start = 1
      @list_tight = false
      @list_delim = :period
      @fence_info = nil
      @fenced = true
      @alignments = nil
      @table_header = false
      @checked = nil
      @extra = nil
      @sourcepos = [0, 0, 0, 0]
      case type
      when :header then @level = 1
      when :code_block then @content = +""; @fence_info = +""; @fenced = false
      when :code then @content = +""; @fence_info = +""
      when :html, :text, :inline_html, :footnote_reference then @content = +""
      when :link, :image then @url = +""; @title = +""
      when :list_item then @list_start = 0
      end
    end
    private :__init

    def __init_row(row, parent)
      @type = row[T_TYPE]
      @content = row[T_CONTENT]
      @url = row[T_URL]
      @title = row[T_TITLE]
      @level = row[T_LEVEL] || 0
      @list_type = row[T_LIST_TYPE] || :bullet_list
      @list_start = row[T_LIST_START] || 0
      @list_tight = row[T_LIST_TIGHT] ? true : false
      @list_delim = row[T_LIST_DELIM] || :period
      @fence_info = row[T_FENCE_INFO]
      @fenced = row[T_FENCED].nil? ? true : row[T_FENCED]
      @alignments = row[T_ALIGNMENTS]
      @table_header = row[T_TABLE_HEADER] ? true : false
      @checked = row[T_CHECKED]
      @extra = row[T_EXTRA]
      @sourcepos = row[T_SOURCEPOS] || [0, 0, 0, 0]
      @parent = parent
      @first_child = @last_child = @next = @previous = nil
      prev = nil
      row[T_CHILDREN].each do |child_row|
        child = Node.__from_row(child_row, self)
        if prev
          prev.__set_next(child)
          child.__set_previous(prev)
        else
          @first_child = child
        end
        prev = child
      end
      @last_child = prev
    end
    private :__init_row

    # A detached deep copy (markly 0.19's `Node#dup`).
    def _dup
      Node.__from_row(__row, nil)
    end

    def __row
      [
        @type, @content, @url, @title, @level, @list_type, @list_start,
        @list_tight, @list_delim, @fence_info, @fenced, @alignments,
        @table_header, @checked, @extra, @sourcepos,
        each.map { |c| c.__row },
      ]
    end
    protected :__row

    def __set_next(node) = @next = node
    def __set_previous(node) = @previous = node
    def __set_parent(node) = @parent = node
    def __set_first_child(node) = @first_child = node
    def __set_last_child(node) = @last_child = node
    protected :__set_next, :__set_previous, :__set_parent, :__set_first_child, :__set_last_child

    # -- rendering ---------------------------------------------------------

    def _render_html(flags, extensions)
      unless flags.is_a?(Integer)
        raise TypeError, "wrong argument type #{flags.class} (expected Integer)"
      end
      unless extensions.is_a?(Array)
        raise TypeError, "wrong argument type #{extensions.class} (expected Array)"
      end
      extensions.each do |ext|
        unless ext.is_a?(Symbol)
          raise TypeError, "extension names should be Symbols; got a #{ext.class}"
        end
        unless Markly.extensions.include?(ext.to_s)
          raise ArgumentError, "extension #{ext} not found\n"
        end
      end
      String.__markly_render_html(__row, flags, extensions)
    end

    def _render_commonmark(flags, width = nil)
      unless flags.is_a?(Integer)
        raise TypeError, "wrong argument type #{flags.class} (expected Integer)"
      end
      unless width.nil? || width.is_a?(Integer)
        raise TypeError, "wrong argument type #{width.class} (expected Integer)"
      end
      String.__markly_render_commonmark(__row, flags, width)
    end

    def _render_plaintext(flags, width = nil)
      unless flags.is_a?(Integer)
        raise TypeError, "wrong argument type #{flags.class} (expected Integer)"
      end
      unless width.nil? || width.is_a?(Integer)
        raise TypeError, "wrong argument type #{width.class} (expected Integer)"
      end
      String.__markly_render_plaintext(__row, flags, width)
    end

    def html_escape_href(str)
      String.__markly_escape_href(str)
    end

    def html_escape_html(str)
      String.__markly_escape_html(str)
    end

    # -- tree surgery ------------------------------------------------------

    # Public: Removes the node from its parent (cmark_node_unlink).
    def delete
      __unlink
      nil
    end

    def __unlink
      if @previous
        @previous.__set_next(@next)
      elsif @parent
        @parent.__set_first_child(@next)
      end
      if @next
        @next.__set_previous(@previous)
      elsif @parent
        @parent.__set_last_child(@previous)
      end
      @parent = nil
      @next = nil
      @previous = nil
      self
    end
    protected :__unlink

    def __check_node(other, what)
      return if other.is_a?(Node)
      raise TypeError, "wrong argument type #{other.class} (expected Markly::Node)"
    end
    private :__check_node

    BLOCK_TYPES = %i[
      document blockquote list list_item code_block html paragraph header
      hrule footnote_definition custom_block table table_header table_row
      table_cell front_matter
    ].freeze
    private_constant :BLOCK_TYPES

    # cmark's `S_can_contain`: which children a node accepts.
    def __can_contain(child)
      node = self
      while node
        return false if node.equal?(child)
        node = node.parent
      end
      return false if child.type == :document
      case @type
      when :document, :blockquote, :footnote_definition, :list_item
        BLOCK_TYPES.include?(child.type) && child.type != :list_item
      when :list
        child.type == :list_item
      when :custom_block
        true
      when :paragraph, :header, :emph, :strong, :link, :image, :custom_inline,
           :strikethrough, :table_cell
        !BLOCK_TYPES.include?(child.type)
      when :table
        child.type == :table_row || child.type == :table_header
      when :table_row, :table_header
        child.type == :table_cell
      else
        false
      end
    end
    protected :__can_contain

    def insert_before(sibling)
      __check_node(sibling, "insert before")
      raise Error, "could not insert before" if @parent.nil? || !@parent.__can_contain(sibling)
      sibling.__unlink
      sibling.__set_parent(@parent)
      sibling.__set_previous(@previous)
      sibling.__set_next(self)
      if @previous
        @previous.__set_next(sibling)
      else
        @parent.__set_first_child(sibling)
      end
      @previous = sibling
      true
    end

    def insert_after(sibling)
      __check_node(sibling, "insert after")
      raise Error, "could not insert after" if @parent.nil? || !@parent.__can_contain(sibling)
      sibling.__unlink
      sibling.__set_parent(@parent)
      sibling.__set_next(@next)
      sibling.__set_previous(self)
      if @next
        @next.__set_previous(sibling)
      else
        @parent.__set_last_child(sibling)
      end
      @next = sibling
      true
    end

    def prepend_child(child)
      __check_node(child, "prepend child")
      raise Error, "could not prepend child" unless __can_contain(child)
      child.__unlink
      child.__set_parent(self)
      child.__set_next(@first_child)
      child.__set_previous(nil)
      if @first_child
        @first_child.__set_previous(child)
      else
        @last_child = child
      end
      @first_child = child
      true
    end

    def append_child(child)
      __check_node(child, "append child")
      raise Error, "could not append child" unless __can_contain(child)
      child.__unlink
      child.__set_parent(self)
      child.__set_previous(@last_child)
      child.__set_next(nil)
      if @last_child
        @last_child.__set_next(child)
      else
        @first_child = child
      end
      @last_child = child
      true
    end

    # Public: Replaces this node with `other` (cmark_node_replace).
    def replace(other)
      __check_node(other, "replace")
      raise Error, "could not replace node" if @parent.nil? || !@parent.__can_contain(other)
      insert_before(other)
      __unlink
      other
    end

    # -- attributes --------------------------------------------------------

    def type_string
      return "tasklist" if @type == :list_item && !@checked.nil?
      TYPE_STRINGS[@type] || @type.to_s
    end

    def source_position
      {
        start_line: @sourcepos[0],
        start_column: @sourcepos[1],
        end_line: @sourcepos[2],
        end_column: @sourcepos[3],
      }
    end

    def string_content
      return nil unless LITERAL_TYPES.include?(@type)
      (@content || "").dup.force_encoding(Encoding::UTF_8)
    end

    def string_content=(s)
      raise TypeError, "wrong argument type #{s.class} (expected String)" unless s.is_a?(String)
      raise Error, "could not set string content" unless LITERAL_TYPES.include?(@type)
      @content = s.dup
      nil
    end

    def url
      raise Error, "could not get url" unless @type == :link || @type == :image
      (@url || "").dup
    end

    def url=(url)
      raise TypeError, "wrong argument type #{url.class} (expected String)" unless url.is_a?(String)
      raise Error, "could not set url" unless @type == :link || @type == :image
      @url = url.dup
      nil
    end

    def title
      raise Error, "could not get title" unless @type == :link || @type == :image
      (@title || "").dup
    end

    def title=(title)
      raise TypeError, "wrong argument type #{title.class} (expected String)" unless title.is_a?(String)
      raise Error, "could not set title" unless @type == :link || @type == :image
      @title = title.dup
      nil
    end

    def header_level
      raise Error, "could not get header_level" unless @type == :header
      @level
    end

    def header_level=(level)
      raise TypeError, "wrong argument type #{level.class} (expected Integer)" unless level.is_a?(Integer)
      raise Error, "could not set header_level" unless @type == :header && (1..6).cover?(level)
      @level = level
      nil
    end

    def list_type
      raise Error, "could not get list_type" unless @type == :list
      @list_type
    end

    def list_type=(list_type)
      raise Error, "invalid list_type" unless list_type == :bullet_list || list_type == :ordered_list
      raise Error, "could not set list_type" unless @type == :list
      @list_type = list_type
      nil
    end

    def list_start
      unless @type == :list && @list_type == :ordered_list
        # The extension formats cmark_node_get_list_type: 1 = bullet, 0 = none.
        raise Error, "can't get list_start for non-ordered list #{@type == :list ? 1 : 0}"
      end
      @list_start
    end

    def list_start=(start)
      raise TypeError, "wrong argument type #{start.class} (expected Integer)" unless start.is_a?(Integer)
      raise Error, "could not set list_start" unless @type == :list && start >= 0
      @list_start = start
      nil
    end

    def list_tight
      raise Error, "can't get list_tight for non-list" unless @type == :list
      @list_tight
    end

    def list_tight=(tight)
      raise Error, "could not set list_tight" unless @type == :list
      @list_tight = tight ? true : false
      nil
    end

    def fence_info
      raise Error, "could not get fence_info" unless @type == :code_block
      (@fence_info || "").dup
    end

    def fence_info=(info)
      raise TypeError, "wrong argument type #{info.class} (expected String)" unless info.is_a?(String)
      raise Error, "could not set fence_info" unless @type == :code_block
      @fence_info = info.dup
      nil
    end

    # markly 0.19: the info string of a code block, an inline code span
    # or the front matter (its format hint).
    def code_info
      unless @type == :code_block || @type == :code || @type == :front_matter
        raise Error, "could not get code_info"
      end
      (@fence_info || "").dup
    end

    def code_info=(info)
      unless info.nil? || info.is_a?(String)
        raise TypeError, "wrong argument type #{info.class} (expected String)"
      end
      unless @type == :code_block || @type == :code || @type == :front_matter
        raise Error, "could not set code_info"
      end
      @fence_info = info&.dup
      nil
    end

    Fence = Struct.new(:character, :length, :indent)

    # markly 0.19: the fence of a fenced code block, nil otherwise.
    def fence
      return nil unless @type == :code_block && @fenced
      char, length, indent = @extra.is_a?(Array) ? @extra : nil
      Fence.new(char || "`", length || 3, indent || 0)
    end

    def table_alignments
      raise Error, "could not get column_count or alignments" unless @type == :table && @alignments
      @alignments.dup
    end

    def tasklist_item_checked?
      @checked ? true : false
    end

    def tasklist_item_checked=(checked)
      raise Error, "could not set tasklist_item_checked" unless @type == :list_item
      @checked = checked ? true : false
      nil
    end

    # The footnote definition a reference points at (cmark links the two
    # when it resolves the footnotes; every other node answers nil).
    def parent_footnote_def
      return nil unless @type == :footnote_reference
      label = @extra.is_a?(Array) ? @extra[2] : nil
      root = self
      root = root.parent while root.parent
      root.walk.find { |n| n.type == :footnote_definition && n.string_content == label }
    end
  end
end
