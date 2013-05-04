test('2185', [when(fast(), skip), reqlib('parallel'), 
              extra_run_opts('+RTS -M16m -RTS'),
              only_ways(['threaded1','threaded2'])], 
        # threaded1 demonstrates the bug: sparks were treated as roots by GC
        multimod_compile_and_run, ['2185',''])
