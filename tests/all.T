test('par001', only_ways(['threaded1', 'threaded2']), 
                compile_and_run, ['-package parallel'])

test('par002', only_ways(['threaded2']),
                compile_and_run, ['-O0 -package parallel'])


test('par003', req_smp, run_command, ['$MAKE -s --no-print-directory par003'])
