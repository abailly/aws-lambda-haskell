process.env[‘PATH’] = process.env[‘PATH’] + ‘:’ + process.env[‘LAMBDA_TASK_ROOT’]

const spawn = require('child_process').spawn;
const main = spawn('./main');

main.stdout.on('data', (data) => {
  console.log(`stdout: ${data}`);
});

main.stderr.on('data', (data) => {
  console.log(`stderr: ${data}`);
});

main.on('error', (err) => {
    console.error(`error: ${err}`);
    process.exit(1);
});

main.on('close', (code) => {
   console.log(`child process pipes closed with code ${code}`);
});

main.on('exit', (code) => {
    console.log(`child process exited with code ${code}`);
    process.exit(code);
});

var ctx;

/**
 * handler for AWS Lambda
 */
exports.handle = function(event, context) {
    ctx = context
    
    main.stdin.write(JSON.stringify({
        'event': event,
        'context': context
    }) + '\n');
}
