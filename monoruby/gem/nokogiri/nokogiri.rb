# nokogiri/nokogiri.rb – monoruby's stand-in for nokogiri's C extension
# (nokogiri.so), loaded by gem/nokogiri/extension.rb in its place.
#
# The extension's classes and native methods are provided by
# src/builtins/nokogiri/ over the bundled libxml2 (libxml2-src, 2.13.8
# with nokogiri's patches): `String.__nokogiri_init` builds the class tree
# (`Nokogiri::XML::Node` and its subclasses, `NodeSet`, `Namespace`,
# `XPathContext`, the `SyntaxError`s) and registers the methods, so the
# gem's own Ruby files (vendored unchanged under gem/nokogiri/) reopen
# them as they do on CRuby. See doc/nokogiri.md for what is implemented.
String.__nokogiri_init

# nokogiri/version/info.rb compares versions with Gem::Version; CRuby has
# rubygems loaded up front, monoruby autoloads it from the CLI but not in
# every embedding (the test harness).
require "rubygems" unless defined?(Gem::Version)
