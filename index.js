const util = require("util");
const exec = util.promisify(require("child_process").exec);
const os = require("os");

module.exports = async function ({ cwd, filePath }) {
    let binary;
    switch (os.platform()) {
        case "darwin":
            binary = "strictly-macos-x64";
            break;
        case "linux":
            binary = "strictly-linux-x64";
            break;

        default:
            throw new Error("Your platform is not supported");
    }
    const { stdout, stderror } = await exec(
        `${__dirname}/dist/${binary} ${filePath}`, {
        cwd
    });

    if (stderror) {
        throw stderror;
    }

    return stdout;
}
