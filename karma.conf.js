const { exec } = require("child_process");

module.exports = function (config) {
    config.set({
        basePath: '.',
        frameworks: ['jasmine'],
        browsers: ["ChromeHeadless"],
        autoWatch: false,
        singleRun: true,
        files: [
            "test/**/*.fl"
        ],
        preprocessors: {
            "**/*.fl": ["frameless"]
        },
        plugins: [
            {
                'preprocessor:frameless': ["factory", function () {
                    return (_, file) => new Promise((resolve, reject) =>
                        exec(`cabal run --verbose=silent frameless-compiler ${file}`, (error, stdout, stderr) => {
                            if (error) {
                                reject(stderr);
                            } else {
                                resolve(stdout);
                            }
                        })
                    )
                }],
            },
            "karma-jasmine",
            "karma-chrome-launcher",
        ]
    });
};