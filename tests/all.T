test('par001', only_ways(['threaded1', 'threaded2']), 
                compile_and_run, ['-package parallel'])

test('conc057', only_ways(['threaded2']),
                compile_and_run, ['-O0 -package parallel'])


test('conc068', normal, run_command, ['$MAKE -s --no-print-directory test068'])
