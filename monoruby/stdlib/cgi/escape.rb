# frozen_string_literal: true
#
# Stub for the `cgi/escape` C extension.
#
# In CRuby, `cgi/escape` is a C extension that provides fast versions of
# `CGI.escape`, `CGI.unescape`, `CGI.escapeHTML`, `CGI.unescapeHTML`,
# `CGI.escapeURIComponent`, and `CGI.unescapeURIComponent`. The
# corresponding pure-Ruby implementations live in `cgi/util`, so we just
# load that here.

require "cgi/util"
