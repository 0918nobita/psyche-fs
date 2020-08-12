const path = require('path');

module.exports = {
    mode: 'development',
    entry: './src/App.fsproj',
    output: {
        path: path.join(__dirname, './public'),
        filename: 'bundle.js',
    },
    devtool: 'eval-source-map',
    devServer: {
        contentBase: './public',
        port: 8080,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: 'fable-loader',
        }],
    }
};
