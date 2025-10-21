import type { StrictlyParseSelector } from "./parse-document/typed-query-selector.js";

////////////////////////
// Core functionality //
////////////////////////

const selectorKey: unique symbol = Symbol();
/**
 * Well-typed algebraic wrapper `document.querySelect`.
 *
 * Motivating assumptions:
 *  - Most of the time, you're doing the same sort of thing with the
 *    same sort of `querySelect` query string.
 *  - It's better to throw an exception if `.querySelect` would
 *    return multiple items than just return the first one.
 *  - It's better to throw an exception if `.querySelect` would
 *    return no items than return null.
 *  - What selector is getting run is a highly-relevant implementation
 *    detail, and worth exposing via the type.
 *  - This all kinda sounds like a profunctor optic.
 */
export type Selector<
  /** The query (of document.querySelect()'s ilk) */
  Query extends string,
  /** What this Selector can inject into its target.
   * Set with .consumer, adjusted with .lmap and .dimap
   */
  Eats,
  /** What this Selector can grab from its target.
   * Adjusted with .map and .dimap
   */
  Yarks,
> = {
  [selectorKey]: SelectorPrivate.Data<Query, Eats, Yarks>;
  /**
   * Build a new `Selector` by applying $subSelector to the
   * children of $this.
   *
   * Note: the resulting `Query` type mirrors the actual
   * implementation, which is a /single/ query select for
   * `${<this's query>} > ${<subSelector's query>}`.
   */
  child<Q2 extends string, E2, Y2>(
    subSelector: Selector<Q2, E2, Y2>,
  ): Selector<`${Query} > ${Q2}`, E2, Y2>;
  /**
   * Alias for `this.child(querySelect(...))`
   */
  child<Q2 extends string>(
    compoundSelector: Q2,
  ): Selector<
    `${Query} > ${Q2}`,
    unknown,
    StrictlyParseSelector<`${Query} > ${Q2}`>
  >;
  map<Y2>(fn: (barf: Yarks) => Y2): Selector<Query, Eats, Y2>;
  consumer<T>(
    consume: (elt: StrictlyParseSelector<Query>, t: T) => void,
  ): Selector<Query, T, Yarks>;
  /**
   * Run this Selector to pull a value
   */
  get(): Yarks;
  (): Yarks;
} & IfNever<
  Eats,
  {},
  {
    lmap<E2>(fn: (food: E2) => Eats): Selector<Query, E2, Yarks>;
    dimap<E2, Y2>(
      mapEats: (food: E2) => Eats,
      mapYarks: (barf: Yarks) => Y2,
    ): Selector<Query, Eats, Y2>;
    /**
     * Run this Selector set a value
     */
    set(food: Eats): void;
  }
>;

function mkSelector<Query extends string, Eats, Yarks>(
  selector: Query,
  resolve: (elt: StrictlyParseSelector<Query>) => Yarks,
  consumer: (elt: StrictlyParseSelector<Query>, food: Eats) => void,
): Selector<Query, Eats, Yarks> {
  const get = () => {
    const elt = querySelectExact(selector) as StrictlyParseSelector<Query>;
    return resolve(elt);
  };
  return Object.assign(get, {
    [selectorKey]: {
      selector,
      resolve: resolve as (elt: Element) => Yarks,
      consumer: consumer as (elt: Element, food: Eats) => void,
    },
    get,
    child: (<Q2 extends string, E2, Y2>(
      subSelector: Q2 | Selector<Q2, E2, Y2>,
    ) => {
      if (typeof subSelector === "string") {
        return querySelect(`${selector} > ${subSelector}`);
      } else {
        return mkSelector(
          `${selector} > ${subSelector[selectorKey].selector}` as `${Query} > ${Q2}`,
          subSelector[selectorKey].resolve as (
            elt: StrictlyParseSelector<`${Query} > ${Q2}`>,
          ) => Y2,
          subSelector[selectorKey].consumer as (
            elt: StrictlyParseSelector<`${Query} > ${Q2}`>,
            food: E2,
          ) => void,
        );
      }
    }) as any,
    map: <Y2>(fn: (barf: Yarks) => Y2) =>
      mkSelector<Query, Eats, Y2>(
        selector,
        (elt) => fn(resolve(elt)),
        consumer,
      ),
    consumer: <T>(consume: (elt: StrictlyParseSelector<Query>, t: T) => void) =>
      mkSelector<Query, T, Yarks>(selector, resolve, consume),
    set: (food: Eats) => {
      const elt = querySelectExact(selector) as StrictlyParseSelector<Query>;
      consumer(elt, food);
    },
    lmap: <E2>(fn: (food: E2) => Eats) =>
      mkSelector<Query, E2, Yarks>(selector, resolve, (elt, food) =>
        consumer(elt, fn(food)),
      ),
    dimap: <E2, Y2>(
      mapEats: (food: E2) => Eats,
      mapYarks: (barf: Yarks) => Y2,
    ) =>
      mkSelector<Query, E2, Y2>(
        selector,
        (elt) => mapYarks(resolve(elt)),
        (elt, food) => consumer(elt, mapEats(food)),
      ),
  }) as Selector<Query, Eats, Yarks>;
}

/**
 * Core building block for building `Selector`s
 */
export function querySelect<K extends string>(
  selectors: K,
): Selector<K, never, StrictlyParseSelector<K>> {
  return mkSelector(
    selectors,
    (a) => a,
    (_elt, _val) => {
      throw Error(
        `Illegal branch; .consumer never called for '${selectors}' querySelect`,
      );
    },
  );
}

type ResolveForm<I> = I extends Selector<infer _, infer __, infer T>
  ? T
  : I extends Record<string, any>
    ? { [K in keyof I]: ResolveForm<I[K]> }
    : I;

type ResolveForms<Args extends unknown[]> = Args extends [
  infer First,
  ...infer Rest,
]
  ? [ResolveForm<First>, ...ResolveForm<Rest>]
  : [];

function resolveSelectors<I>(s: I): ResolveForm<I> {
  if (typeof s === "function" && selectorKey in s) {
    return (s as unknown as Selector<string, any, never>).get();
  } else if (typeof s === "object" && !Array.isArray(s) && s !== null) {
    return Object.fromEntries(
      Object.entries(s).map(([k, v]) => [k, resolveSelectors(v)]),
    ) as ResolveForm<I>;
  } else {
    return s as ResolveForm<I>;
  }
}

type ResolvedBy<T> = T extends unknown[]
  ? T extends [infer Head, ...infer Rest]
    ? [ResolvedBy.Impl<Head>, ...ResolvedBy<Rest>]
    : T
  : ResolvedBy.Impl<T>;

namespace ResolvedBy {
  export type Impl<T> = T extends Selector<any, infer Out, any>
    ? IfNever<Out, "ERROR: selector cannot be fed any value", Out>
    : T extends Record<string, any>
      ? { [K in keyof T]: Impl<T[K]> }
      : "ERROR: Not a selector";
}

type ResolversFor<S> = S extends unknown[]
  ? S extends [infer Head, ...infer Rest]
    ? [ResolversFor.Impl<Head>, ...ResolversFor<Rest>]
    : S
  : ResolversFor.Impl<S>;

namespace ResolversFor {
  export type Impl<S> = S extends Record<string, any>
    ? { [K in keyof S]?: Impl<S[K]> } | Selector<string, S, unknown>
    : Selector<string, S, unknown>;
}

export function pipeInto<S>(s: S): <T extends ResolvedBy<S>>(t: T) => void {
  return (t) => {
    (Array.isArray(s)
      ? s.flatMap((val, idx) =>
          pipeIntoKernel(`arg[${idx}]`, val, (t as any[])[idx]),
        )
      : pipeIntoKernel(`arg`, s, t as any)
    ).forEach((cb) => cb());
  };
}

export namespace DocumentPipeline {
  export type For<Arg1, Rest extends unknown[]> = {
    through: <Fn extends (...args: ResolveForms<[Arg1, ...Rest]>) => unknown>(
      fn: Fn,
    ) => Through<ReturnType<Fn>>;
  };
  export type Through<T> = {
    map<U>(fn: (t: T) => U): Through<U>;
    value(): T;
    into: <O extends ResolversFor<T>>(out: O) => void;
  };
}

function mkThrough<T>(getT: () => T): DocumentPipeline.Through<T> {
  return {
    value: getT,
    map: (fn) => mkThrough(() => fn(getT())),
    into: <S extends ResolversFor<T>>(s: S) =>
      pipeInto(s)(getT() as ResolvedBy<S>),
  };
}

/**
 * Simple sample:
 * ```typescript
 * const form = querySelect("form");
 *
 * form().addEventListener("submit", () => pipeDocument(
 *   input.text("#firstname"),
 *   input.text("#lastname"),
 * )
 *   .through((first, last) => `${last}, ${first}`)
 *   .into(
 *     form.child(input.hidden("#fullName")),
 *   ));
 * ```
 */
export function pipeDocument<Arg1, Rest extends unknown[]>(
  first: Arg1,
  ...rest: Rest
): DocumentPipeline.For<Arg1, Rest> {
  return {
    through: <Fn extends (...args: ResolveForms<[Arg1, ...Rest]>) => unknown>(
      fn: Fn,
    ) =>
      mkThrough<ReturnType<Fn>>(
        () =>
          fn(
            ...([
              resolveSelectors(first),
              ...rest.map(resolveSelectors),
            ] as unknown as ResolveForms<[Arg1, ...Rest]>),
          ) as ReturnType<Fn>,
      ),
  };
}

/////////////
// helpers //
/////////////

/**
 * Helper type for cases where we want to allow
 * trailers to narrow a query selection, but not
 * combinators that change the return type of the
 * query.
 *
 * For example, if we have a helper function like:
 *
 * ```javascript
 * function videoTime(compoundSelector) {
 *   return querySelect('video${compoundSelector === undefined ? "" : compoundSelector}')
 *     .map(videoElt => videoElt.currentTime)
 *     .consume((videoElt, time) => { videoElt.currentTime = time });
 * }
 * ```
 *
 * We want to allow `compoundSelector` to select video elements with a specific id
 * or property, but something like `videoTime(" > p#fallbackText")` would be problematic,
 * as the result of the query selector is no longer a video element.
 *
 * Cases like this is where `SelectorOf` comes in handy; it can constrain the compound
 * selector elements such that if they change the selected element type into something
 * not assignable to `Elt`, the `SelectorOf` type resolves to `never` and forces a
 * compilation error.
 *
 * I.e.:
 *
 * ```typescript
 * declare function
 *   videoTime<CompoundSelector extends string>
 *   (compoundSelector: CompoundSelector):
 *   SelectorOf<'video${CompoundSelector}', HTMLVideoElement, number, number>
 *
 * // tsc error:
 * // SelectorOf.Error<"Provided query selector 'video > p#fallbackText' resulted in an incompatible element type"> has no `get` property
 * const evil = videoTime(" > p#fallbackText").get();
 * ```
 */
export type SelectorOf<
  Query extends string,
  Elt extends Element,
  Eats,
  Yarks,
> = StrictlyParseSelector<Query> extends Elt
  ? Selector<Query, Eats, Yarks>
  : SelectorOf.TypeError<`Provided query selector '${Query}' resulted in an incompatible element type`>;

export namespace SelectorOf {
  declare const typeErrorSym: unique symbol;
  /**
   * Just here to label compiler errors
   */
  export interface TypeError<T> {
    [typeErrorSym]: T;
  }
}

function inputFn<CompoundSelector extends string>(
  compoundSelector: CompoundSelector,
): SelectorOf<`input${CompoundSelector}`, HTMLInputElement, string, string>;
function inputFn<CompoundSelector extends string>(
  compoundSelector: CompoundSelector,
): Selector<`input`, string, string>;
function inputFn(compoundSelector?: string | undefined): any {
  return querySelect(
    `input[type="hidden"]${compoundSelector === undefined ? "" : compoundSelector}`,
  )
    .map((elt) => elt.value)
    .consumer<string>((elt, str) => {
      elt.value = str;
    });
}

function inputHidden<CompoundSelector extends string>(
  compoundSelector: CompoundSelector,
): SelectorOf<`input[type="hidden"]`, HTMLInputElement, string, string>;
function inputHidden(): Selector<`input[type="hidden"]`, string, string>;
function inputHidden(compoundSelector?: string | undefined): any {
  return inputFn(
    `[type="hidden"]${compoundSelector === undefined ? "" : compoundSelector}`,
  );
}

function inputText<CompoundSelector extends string>(
  compoundSelector: CompoundSelector,
): SelectorOf<`input[type="text"]`, HTMLInputElement, string, string>;
function inputText(): Selector<`input[type="text"]`, string, string>;
function inputText(compoundSelector?: string | undefined): any {
  return inputFn(
    `[type="text"]${compoundSelector === undefined ? "" : compoundSelector}`,
  );
}

export const input = Object.assign(inputFn, {
  hidden: inputHidden,
  text: inputText,
});

export const ignore: Selector<`:root`, unknown, undefined> = mkSelector(
  ":root",
  (_elt) => undefined,
  (_elt, _food) => {},
);

////////////////////////////
// Implementation details //
////////////////////////////

// query select that yells when it doesn't get
// exactly 1 element.
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

type IfNever<T, WhenTrue, WhenFalse> = [T] extends [never]
  ? WhenTrue
  : WhenFalse;

namespace SelectorPrivate {
  export type Data<out Query extends string, Eats, Yarks> = {
    selector: Query;
    /* Note: `Element` in these types /should/ be StrictlyParseSelector<Query>,
     * but this causes typescript to erroneously infer `Query` as contravariant
     * due to showing up in a function argument, when in practice the `Query`
     * type paramater should only be covariant, not bivariant.
     *
     * Bivariance in `Query` means that a `Selector<'index', ...>` is not assignable
     * to the type `Selector<string, ...>`, which breaks this library's ergonomics.
     */
    resolve: (elt: Element) => Yarks;
    consumer: (elt: Element, food: Eats) => void;
  };
}

function pipeIntoKernel<S, T extends ResolvedBy<S>>(
  loc: string,
  s: S,
  t: T,
): Array<() => void> {
  if (typeof s === "function" && selectorKey in s) {
    return [() => (s as any).set(t)];
  } else if (typeof s === "object" && !Array.isArray(s) && s !== null) {
    return Object.entries(s).flatMap(([k, v]) =>
      pipeIntoKernel(`${loc}.${k}`, v, t[k as keyof T] as any),
    );
  } else {
    throw Error(`Expected Selector at path ${loc}; found ${typeof s}`);
  }
}
