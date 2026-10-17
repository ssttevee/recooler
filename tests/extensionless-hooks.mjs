// easy-jsx-html-engine imports its own modules without file extensions, which bundlers accept but
// node's esm loader does not, so retry relative imports with a `.js` extension.
export async function resolve(specifier, context, nextResolve) {
  try {
    return await nextResolve(specifier, context);
  } catch (err) {
    if (err?.code === "ERR_MODULE_NOT_FOUND" && /^\.\.?\//.test(specifier)) {
      return nextResolve(`${specifier}.js`, context);
    }

    throw err;
  }
}
