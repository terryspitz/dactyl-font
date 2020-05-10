// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");

module.exports = {
    mode: "development",
    entry: "./src/explorer.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8080,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            // use: {
            //     loader: "fable-loader",
            //     options: {
            //         cli: {
            //             // This should be the path to your local clone of Fable
            //             path: "../../Fable/src/Fable.Cli"
            //         },
            //         DEBUG: 1
            //     }
            // }
            use: "fable-loader"
        }]
    }
}