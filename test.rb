  def release
    @@release[self] ||= if prerelease?
      segments = self.segments
      segments.pop while segments.any? {|s| String === s }
      self.class.new segments.join(".")
    else
      self
    end
  end