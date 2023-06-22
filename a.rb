      def initialize(argv)
        @argv = argv.dup
        @options = DEFAULT_OPTIONS.dup
        parse_option until @argv.empty?
        error "ROM file is not given" unless @options[:romfile]
      rescue Invalid => e
        puts "[FATAL] #{ e }"
        exit 1
      end