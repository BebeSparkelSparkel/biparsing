module.exports = {
    "env": {
        "browser": true,
        "commonjs": true,
        "es6": true,
    },
    "extends": "eslint:recommended",
    "parserOptions": {
        "ecmaVersion": 2017,
    },
    "globals": {
      "it": "readonly",
      "describe": "readonly",
    },
    "rules": {
        // "indent": [
        //     "error",
        //     2,
        //     {
        //         "VariableDeclarator": { "var": 2, "let": 2, "const": 3 },
        //         "SwitchCase": 1,
        //     },
        // ],
        "linebreak-style": [
            "error",
            "unix"
        ],
        "quotes": [
            "error",
            "single"
        ],
        "semi": [
            "error",
            "never",
            { "beforeStatementContinuationChars": "always" },
        ],
        "no-inner-declarations": ["off"],
        "guard-for-in": "error",
        "no-var": "error",
        "prefer-const": ["error", {
            "destructuring": "any",
            "ignoreReadBeforeAssign": false
        }],
        "comma-dangle": ['error', "always-multiline"],
        "key-spacing": ['error'],
        "quote-props": ['error', "as-needed"],
        "eqeqeq": ['error', "always"],
        "no-plusplus": ['error', {"allowForLoopAfterthoughts": true}],
        "strict": ["error", "global"],
        "eol-last": ["error", "always"],
        "prefer-destructuring": ["error"],
        "no-trailing-spaces": ["error"],
        "no-throw-literal": ["error"],
    },
}
