const spawn = require('child_process').spawn;
const main = spawn('./main');

main.stdout.on('data', (data) => {
  console.log(`stdout: ${data}`);
});

main.stderr.on('data', (data) => {
  console.log(`stderr: ${data}`);
});

main.on('error', (err) => {
  console.log(`error: ${err}`);
});

main.on('close', (code) => {
  console.log(`child process pipes closed with code ${code}`);
});

main.on('exit', (code) => {
  console.log(`child process exited with code ${code}`);
});
