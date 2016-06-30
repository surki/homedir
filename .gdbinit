set auto-load safe-path /

set history filename ~/.gdb_history
set history save on
set history expansion on
# For Mono/Unity
handle SIGXCPU SIG33 SIG35 SIGPWR nostop noprint

define evalr
  call(rb_p(rb_eval_string_protect($arg0,(int*)0)))
end
document evalr
   Evaluate an arbitrary Ruby expression from current gdb context.
end

# For STL
# python
# import sys
# sys.path.insert(0, '/home/suresh/src/gcc/libstdc++-v3/python')
# from libstdcxx.v6.printers import register_libstdcxx_printers
# register_libstdcxx_printers (None)
# end

# define mono_backtrace
#  select-frame 0
#  set $i = 0
#  while ($i < $arg0)
#    set $foo = (char*) mono_pmip ($pc)
#    if ($foo)
#      printf "#%d %p in %s\n", $i, $pc, $foo
#    else
#      frame
#    end
#    up-silently
#    set $i = $i + 1
#  end
# end

# define mono_stack
#  set $mono_thread = mono_thread_current ()
#  if ($mono_thread == 0x00)
#    printf "No mono thread associated with this thread\n"
#  else
#    set $ucp = malloc (sizeof (ucontext_t))
#    call (void) getcontext ($ucp)
#    call (void) mono_print_thread_dump ($ucp)
#    call (void) free ($ucp)
#  end
# end
