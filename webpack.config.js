const path = require("path")
const HtmlWebpackPlugin = require("html-webpack-plugin")
const UglifyJSPlugin = require("uglifyjs-webpack-plugin")
const ScriptExtHtmlWebpackPlugin = require("script-ext-html-webpack-plugin")
const CopyWebpackPlugin = require("copy-webpack-plugin")

module.exports = {
  entry: {
    app: ["./src/index.js", "./src/logo.png"],
  },

  output: {
    path: path.resolve(__dirname + "/dist"),
    filename: "[name]-[hash].js",
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.template.ejs",
      favicon: "./favicon.ico",
      inject: "body",
    }),
    new ScriptExtHtmlWebpackPlugin({
      defaultAttribute: "async",
    }),
    new UglifyJSPlugin({
      mangle: true,
    }),
    new CopyWebpackPlugin([{ from: "assets/img", to: "assets/img" }]),
  ],
  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: "file-loader?name=[name].[ext]",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader?verbose=true&warn=true&debug=false",
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: "url-loader?limit=10000&mimetype=application/font-woff",
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: "file-loader",
      },
      {
        test: /\.png$/,
        loader: "file-loader?name=[name].[ext]",
      },
    ],

    noParse: /\.elm$/,
  },

  devServer: {
    inline: true,
    stats: { colors: true },
  },
}
