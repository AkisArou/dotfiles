"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getRepositoryName = void 0;
const git_url_parse_1 = __importDefault(require("git-url-parse"));
/**
 * Get a repository full name (owner and repo, plus organization for ADO/VSO) from a repository URL,
 * including special handling for the many ADO/VSO URL formats.
 *
 * Examples:
 * - returns `microsoft/workspace-tools` for `https://github.com/microsoft/workspace-tools.git`
 * - returns `foo/bar/some-repo` for `https://dev.azure.com/foo/bar/_git/some-repo`
 */
function getRepositoryName(url) {
    var _a;
    try {
        // Mostly use this standard library, but fix some VSO/ADO-specific quirks to account for the
        // fact that all of the following URLs should be considered to point to the same repo:
        // https://foo.visualstudio.com/bar/_git/some-repo
        // https://foo.visualstudio.com/DefaultCollection/bar/_git/some-repo
        // https://user:token@foo.visualstudio.com/DefaultCollection/bar/_git/some-repo
        // https://foo.visualstudio.com/DefaultCollection/bar/_git/_optimized/some-repo
        // foo@vs-ssh.visualstudio.com:v3/foo/bar/some-repo
        // https://dev.azure.com/foo/bar/_git/some-repo
        // https://dev.azure.com/foo/bar/_git/_optimized/some-repo
        // https://user@dev.azure.com/foo/bar/_git/some-repo
        // git@ssh.dev.azure.com:v3/foo/bar/some-repo
        let fixedUrl = url.replace("/_optimized/", "/").replace("/DefaultCollection/", "/");
        const parsedUrl = (0, git_url_parse_1.default)(fixedUrl);
        const isVSO = fixedUrl.includes(".visualstudio.com");
        const isADO = fixedUrl.includes("dev.azure.com");
        if (!isVSO && !isADO) {
            return parsedUrl.full_name;
        }
        // As of writing, ADO and VSO SSH URLs are parsed completely wrong
        const sshMatch = parsedUrl.full_name.match(/(vs-ssh\.visualstudio\.com|ssh\.dev\.azure\.com):v\d+\/([^/]+)\/([^/]+)/);
        if (sshMatch) {
            return `${sshMatch[2]}/${sshMatch[3]}/${parsedUrl.name}`;
        }
        // As of writing, full_name is wrong for enough variants of ADO and VSO URLs that it
        // makes more sense to just build it manually.
        let organization = parsedUrl.organization;
        if (!organization && isVSO) {
            // organization is missing or wrong for VSO
            organization = (_a = parsedUrl.resource.match(/([^.@]+)\.visualstudio\.com/)) === null || _a === void 0 ? void 0 : _a[1];
        }
        return `${organization}/${parsedUrl.owner}/${parsedUrl.name}`;
    }
    catch (err) {
        return "";
    }
}
exports.getRepositoryName = getRepositoryName;
//# sourceMappingURL=getRepositoryName.js.map