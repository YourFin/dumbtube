import type { StrictlyParseSelector } from "./typed-query-selector.js";

const selectorKey: unique symbol = Symbol();
export type Selector<Query extends string, T> = {
  [selectorKey]: Selector.Private<Query, T>;
  child: <ChildQuery extends string, ChildResult>(
    subSelector: Selector<ChildQuery, ChildResult>,
  ) => Selector<`${Query} > ${ChildQuery}`, ChildResult>;
  map: <U>(fn: (t: T) => U) => Selector<Query, U>;
  (): T;
};

namespace Selector {
  export type Private<Query extends string, T> = {
    selector: Query;
    resolve: (elt: StrictlyParseSelector<Query>) => T;
  };
}

function mkSelector<Query extends string, T>(
  selector: Query,
  resolve: (elt: StrictlyParseSelector<Query>) => T,
): Selector<Query, T> {
  let ret = () => {
    const elt = querySelectExact(selector) as StrictlyParseSelector<Query>;
    return resolve(elt);
  };
  return Object.assign(ret, {
    [selectorKey]: { selector, resolve },
    child: <ChildQuery extends string, ChildResult>(
      subSelector: Selector<ChildQuery, ChildResult>,
    ) => {
      return mkSelector(
        `${selector} > ${subSelector[selectorKey].selector}` as ChildQuery,
        subSelector[selectorKey].resolve,
      ) as unknown as Selector<`${Query} > ${ChildQuery}`, ChildResult>;
    },
    map: <U>(fn: (t: T) => U) =>
      mkSelector<Query, U>(selector, (elt) => fn(resolve(elt))),
  });
}

function querySelectExact<K extends string>(
  selectors: K,
): StrictlyParseSelector<K> {
  const result = document.querySelectorAll(selectors);
  if (result.length === 0) {
    throw new Error(`${selectors} did not match anything`);
  } else if (result.length !== 1) {
    throw new Error(
      `${selectors} matched ${result.length} elements; expected 1`,
    );
  } else {
    return result.item(0) as StrictlyParseSelector<K>;
  }
}

export function querySelect<K extends string>(
  selectors: K,
): Selector<K, StrictlyParseSelector<K>> {
  return mkSelector(selectors, (a) => a);
}

type ResolveForm<I> = I extends Selector<any, infer T>
  ? T
  : I extends Record<string, any>
    ? { [K in keyof I]: ResolveForm<I[K]> }
    : I;

export function parseDocument<Structure>(s: Structure): ResolveForm<Structure> {
  if (typeof s === "function" && selectorKey in s) {
    return (s as unknown as Selector<string, never>)();
  } else if (typeof s === "object" && !Array.isArray(s) && s !== null) {
    return Object.fromEntries(
      Object.entries(s).map(([k, v]) => [k, parseDocument(v)]),
    ) as ResolveForm<Structure>;
  } else {
    return s as ResolveForm<Structure>;
  }
}
