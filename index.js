const os = require("os");
const util = require("util");
const exec = util.promisify(require("child_process").exec);

module.exports = function ({ cwd }) {
    return async function ({ filePath }) {
        let binary;
        switch (os.platform()) {
            case "darwin":
                binary = "macos-x64/strictly";
                break;
            case "linux":
                binary = "linux-x64/strictly";
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
}
