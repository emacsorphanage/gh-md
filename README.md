# gh-md - Render markdown using the Github api

*Author:* Mario Rodas <marsam@users.noreply.github.com><br>
*Version:* 0.1.1<br>

[![Travis build status](https://travis-ci.org/emacs-pe/gh-md.el.png?branch=master)](https://travis-ci.org/emacs-pe/gh-md.el)

Render markdown using the [Github API](https://developer.github.com/v3/markdown/).

## Usage

After install `gh-md.el` you can use the functions
`gh-md-render-region` and `gh-md-render-buffer` to generate a
preview of the markdown content of a buffer.

![gh-md.el screenshot](screenshot.png)

### Note

If you get an error on the **Messages** buffer similar to
`peculiar error: "connect" host, "api.github.com" :service,443`,
then you might need to run this on your terminal:

```
git config --global --unset http.proxy
git config --global --unset https.proxy
```

---
Converted from `gh-md.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
