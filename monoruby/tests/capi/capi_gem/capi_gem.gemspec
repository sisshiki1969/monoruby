# A gem with a C extension, as `gem install` sees one. Built and
# installed on CRuby and on monoruby by tests/mkmf.rs.
Gem::Specification.new do |s|
  s.name = "capi_gem"
  s.version = "0.1.0"
  s.summary = "mkmf fixture for monoruby's C API"
  s.authors = ["monoruby"]
  s.license = "MIT"
  s.files = Dir["lib/**/*.rb", "ext/**/*.{c,rb}"]
  s.extensions = ["ext/capi_gem/extconf.rb"]
  s.require_paths = ["lib"]
  s.required_ruby_version = ">= 3.0"
end
