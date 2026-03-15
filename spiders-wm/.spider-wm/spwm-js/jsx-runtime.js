const flattenChildren = (input, out) => {
  for (const child of input) {
    if (Array.isArray(child)) {
      flattenChildren(child, out);
      continue;
    }
    if (child === false || child === null || child === undefined) {
      continue;
    }
    out.push(child);
  }
};

export const Fragment = Symbol("spider-wm.fragment");

function createNode(type, props, ...children) {
  const normalizedChildren = [];
  const nextProps = props || {};

  flattenChildren(children, normalizedChildren);
  if (type === Fragment) {
    return normalizedChildren;
  }
  if (typeof type === "function") {
    return type({
      ...nextProps,
      children: normalizedChildren,
    });
  }
  return {
    type,
    props: nextProps,
    children: normalizedChildren,
  };
}

export function sp(type, props, ...children) {
  return createNode(type, props, ...children);
}

export function jsx(type, props, key) {
  const nextProps = props || {};
  const children = Object.prototype.hasOwnProperty.call(nextProps, "children")
    ? [nextProps.children]
    : [];
  const runtimeProps = { ...nextProps };

  void key;
  delete runtimeProps.children;
  return createNode(type, runtimeProps, ...children);
}

export function jsxs(type, props, key) {
  return jsx(type, props, key);
}
