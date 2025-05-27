puts %r[\A
    (?<seg>#{100}){0}
    (?<Gem::URI>
      (?<scheme>#{101}):
      (?<hier-part>//
        (?<authority>
          (?:(?<userinfo>#{102})@)?
          (?<host>#{103})
          (?::(?<port>\d*+))?
        )
        (?<path-abempty>(?:/\g<seg>*+)?)
      | (?<path-absolute>/((?!/)\g<seg>++)?)
      | (?<path-rootless>(?!/)\g<seg>++)
      | (?<path-empty>)
      )
      (?:\?(?<query>[^\#]*+))?
      (?:\#(?<fragment>#{104}))?
    )\z]