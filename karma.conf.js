const util = require('util');
const exec = util.promisify(require('child_process').exec);

module.exports = function (config) {
    config.set({
        basePath: ".",
        frameworks: ["jasmine"],
        browsers: ["ChromeHeadless"],
        autoWatch: false,
        singleRun: true,
        files: [
            "test/components/host/base/*.fl",
            "test/components/host/siblings/*.fl",
            "test/components/host/withtext/*.fl",
            "test/components/host/nested/*.fl",
            "test/components/text/**/*.fl",
            "test/components/helper/**/*.fl",
            "test/**/*.js"
        ],
        preprocessors: {
            "**/*.fl": ["frameless"]
        },
        plugins: [
            {
                "preprocessor:frameless": ["factory", function () {
                    return async (_, file) => {
                        file.path = file.originalPath.replace(/\.fl$/, '.js');

                        const { stdout, stderror } = await exec(`cabal v2-run --verbose=silent frameless-compiler ${file.originalPath}`);
                        if (stderror) {
                            throw stderror;
                        }

                        return stdout;
                    }
                }],
            },
            "karma-jasmine",
            "karma-chrome-launcher",
        ]
    });
};