require "date"
require "yaml"
require "json"
require "json/add/date"
d = Date.new(2001, 2, 3)
r = [
  YAML.dump(d), YAML.load("--- 2001-02-03\n", permitted_classes: [Date]), YAML.load("a: 2001-02-03 04:05:06 Z", permitted_classes: [Time])["a"].utc,
  YAML.load(YAML.dump([d, DateTime.new(2001, 2, 3, 4, 5, 6)]), permitted_classes: [Date, DateTime, Time]).map(&:to_s),
  d.to_json, JSON.parse(d.to_json, create_additions: true), JSON.parse(d.to_json).to_a,
]
r.each { |v| p v }
