const { exec } = require("child_process");

module.exports = function (config) {
    config.set({
        basePath: ".",
        frameworks: ["jasmine"],
        browsers: ["ChromeHeadless"],
        autoWatch: false,
        singleRun: true,
        files: [
            "test/components/host/base/index.fl",
            "test/components/host/siblings/index.fl",
            "test/**/*.js"
        ],
        preprocessors: {
            "**/*.fl": ["frameless"]
        },
        plugins: [
            {
                "preprocessor:frameless": ["factory", function () {
                    return (_, file) => new Promise((resolve, reject) => {
                        file.path = file.originalPath.replace(/\.fl$/, '.js');

                        return exec(`cabal run --verbose=silent frameless-compiler ${file.originalPath}`, (error, stdout, stderr) => {
                            if (error) {
                                reject(stderr);
                            } else {
                                resolve(stdout);
                            }
                        });
                    })
                }],
            },
            "karma-jasmine",
            "karma-chrome-launcher",
        ]
    });
};