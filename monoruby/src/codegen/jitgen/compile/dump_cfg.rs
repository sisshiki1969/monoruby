use super::*;

pub(super) fn dump_cfg(
    func: &ISeqInfo,
    store: &Store,
    bb_begin: BasicBlockId,
    bb_end: BasicBlockId,
) {
    let mut s = format!(
        r###"digraph graph_name {{
  graph [
    charset = "UTF-8";
    label = "{}",
    labelloc = "t",
    labeljust = "c",
    bgcolor = "#343434",
    fontcolor = white,
    fontsize = 20,
    rankdir = TB,
    margin = 0.2,
    splines = spline,
    nodesep = 0.8,
    ranksep = 1.1
  ];

  node [
    colorscheme = "accent8"
    shape = box,
    style = "solid,filled",
    fontsize = 16,
    fontcolor = 5,
    fontname = "Consolas",
    color = 5,
    fillcolor = 4,
  ];

  edge [
    style = solid,
    fontsize = 14,
    fontcolor = white,
    fontname = "Migu 1M",
    color = white,
    labelfloat = true,
    labeldistance = 2.5,
    labelangle = 70
  ];"###,
        store.func_description(func.func_id())
    );
    s += "\n";
    for bbid in bb_begin..=bb_end {
        s += &format!("  {:?} [\n    shape=record\n    label=\"{{{:?}", bbid, bbid);
        let BasicBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
        for bc in begin..=end {
            if let Some(inst) = func.trace_ir(store, bc).format(store) {
                s += "|";
                let html = html_escape::encode_text(&inst)
                    .replace('|', "\\|")
                    .replace('"', "\\\"");
                s += &format!("{} {}\\l", bc, html);
            }
        }
        s += "}\"\n  ];\n";
    }

    for bbid in bb_begin..=bb_end {
        let entry = &func.bb_info[bbid];
        for succ in &entry.succ {
            s += &format!("  {:?} -> {:?} [headport = n, tailport = s];\n", bbid, succ);
        }
    }

    s += "}\n";
    let path = std::path::PathBuf::from(".cfg");
    match path.try_exists() {
        Ok(true) => {}
        _ => std::fs::create_dir(&path).unwrap(),
    }
    std::fs::write(path.join(format!("fid-{}.dot", func.func_id().get())), s).unwrap();
}
