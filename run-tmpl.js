const spawn = require('child_process').spawn;
const main = spawn('./$$main$$', { stdio: ['pipe', 'pipe', process.stderr] });

main.stdout.on('data', function(data) {
    console.log('stdout: ' + data);
});

main.on('close', function(code) {
    console.log('child process pipes closed with code '+ code);
});

var ctx;

/**
 * handler for AWS Lambda
 */
exports.handle = function(event, context, callback) {
    process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT']
    // help resolve dynamic libraries packaged with function
    process.env['LD_LIBRARY_PATH'] = process.env['LAMBDA_TASK_ROOT']
    ctx = context

    console.log("sending data to $$main$$:\n" + JSON.stringify(event));
    console.log("$$main$$ pid is " + main.pid);
    
    main.on('exit', function(code){
        callback(null, 'child process exited with code ' + code); 
    });

    main.on('error', function(err) {
        console.error('error: ' + err);
        callback(err, 'child process exited with error: ' + err); 
    });

    main.stdin.write(JSON.stringify({
        'event': event,
        'context': context
    }) + '\n');
}
