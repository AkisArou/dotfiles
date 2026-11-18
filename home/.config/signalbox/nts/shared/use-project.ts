// nts's project data: the kit's, with its status pages' readings. Their trees (Machine)
// and landings (Landing) are work on issues too, so they mark issues in progress.
import { useProject as useKitProject } from "@signalbox/kit/shared/use-project";
import { useNts } from "../status/use-nts.ts";

export { current } from "@signalbox/kit/shared/use-project";

export function useProject() {
  const nts = useNts();
  const project = useKitProject({
    claims: [...(nts.page("Machine")?.claims ?? []), ...(nts.page("Landing")?.claims ?? [])],
  });
  return { ...project, nts };
}
export type Project = ReturnType<typeof useProject>;
