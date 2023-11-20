/**
 * Helper that logs an error to `console.warn` if `process.env.VERBOSE` is set.
 * This should be replaced with a proper logging system eventually.
 */
export declare function logVerboseWarning(description: string, err?: unknown): void;
