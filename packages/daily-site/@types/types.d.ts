/// <reference types="vite/client" />

/**
* Available branch types that can be shown on the daily site.
* Defined here: https://github.com/optimizely/axiom-daily-sites/blob/master/.scripts/update_view.mjs#L50
*/
type BranchName = "main" | "release" | "user" | "other";
type TDailyAction = "created" | "deleted" | "page-push";

interface IBranchData {
    name: string;
    slug: string;
    jira?: string;
    created: string;
}

interface IDailyMetaData {
    ID: number;
    PROJECTS: IDailyProject[];
    REPOSITORY: string;
}

interface IDailyProject {
    name: string;
    repository?: string;
    branches: {
        main: Array<string | IBranchData>;
        release: Array<string | IBranchData>;
        feature: Array<string | IBranchData>;
        user: Array<string | IBranchData>;
        other: Array<string | IBranchData>;
    };
}
