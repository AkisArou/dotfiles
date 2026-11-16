import { createContext, createElement, useContext } from "react";
import type { ReactNode } from "react";

const Endpoint = createContext<string | undefined>(undefined);
export function TerminalProvider({ url, children }: { url: string; children: ReactNode }) {
  return createElement(Endpoint.Provider, { value: url }, children);
}
export const useTerminalEndpoint = () => useContext(Endpoint);
