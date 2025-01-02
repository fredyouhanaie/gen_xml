
-define(Arguments,
        [ #{ name => verbose, long => "-verbose", short => $v,
             type => boolean, action => count,
             help => "be verbose, can use multiple times for warning to debug" },
          #{ name => help, long => "-help", short => $h,
             type => boolean,
             help => "display help/usage information" },
          #{ name => file, nargs => 1 }
        ] ).

-define(Commands,
        #{ "null"    => #{ help => "run the null callback module",
                           handler => fun genxml_cli:do_null/1 },
           "counts"  => #{ help => "run the counts callback module",
                           handler => fun genxml_cli:do_counts/1 },
           "ets"     => #{ help => "run the ets callback module",
                           handler => fun genxml_cli:do_ets/1 },
           "paths"   => #{ help => "run the paths callback module",
                           handler => fun genxml_cli:do_paths/1 }
         } ).
