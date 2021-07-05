"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.extractCommitData = exports.slug = exports.sameBranch = exports.getBranchType = exports.branch_types = exports.main_branches = exports.getJiraTag = exports.getCreatedDate = exports.actionHandler = exports.script_tag = exports.getProject = void 0;
function getProject(project_name, repo, projects) {
    var project = projects.find(function (project) { return project.name === project_name; });
    if (!project) {
        project = {
            name: project_name,
            branches: {
                main: [],
                release: [],
                feature: [],
                user: [],
                other: []
            }
        };
        projects.push(project);
    }
    project.repository = repo;
    return project;
}
exports.getProject = getProject;
var script_tag = function (_id) {
    return "<script id=\"daily_site_view_model\" src=\"./www/view_model." + _id + ".js\"></script>";
};
exports.script_tag = script_tag;
function actionHandler(action, project, branch_name) {
    var slug_name = slug(branch_name);
    var slug_parts = slug_name.split("-");
    var jira = getJiraTag(slug_parts);
    var type = getBranchType(slug_parts);
    var branch_data = {
        name: branch_name.includes("/") ? branch_name : "N/A",
        slug: slug_name,
        created: getCreatedDate(),
        jira: jira,
    };
    switch (action.toLowerCase()) {
        case "created":
        case "updated":
            if (!project.branches[type].find(function (_branch) { return sameBranch(_branch, branch_data); })) {
                project.branches[type].push(branch_data);
                return true;
            }
            return false;
        case "deleted":
            if (project.branches[type].find(function (_branch) { return sameBranch(_branch, branch_data); })) {
                project.branches[type] = project.branches[type].filter(function (_branch) { return !sameBranch(_branch, branch_data); });
                return true;
            }
            return false;
        case "page-push":
            return true;
        default:
            return false;
    }
}
exports.actionHandler = actionHandler;
function getCreatedDate() {
    var date = new Date();
    var year = date.getFullYear();
    var month = date.getMonth() + 1;
    var day = date.getDate();
    var hour = date.getHours();
    var minute = date.getMinutes();
    // Write the date in yyyy-mm-dd hh:mm.
    return year + "-" + (month < 10 ? "0" + month : month) + "-" + (day < 10 ? "0" + day : day) + " " + hour + ":" + minute;
}
exports.getCreatedDate = getCreatedDate;
function getJiraTag(slug_parts) {
    var jira_tag = "N/A";
    if (slug_parts.includes("ax")) {
        // We have a supported jira tag.
        jira_tag = slug_parts
            .filter(function (part, index) { return part === "ax" || slug_parts[index - 1] === "ax"; })
            .map(function (part) { return part.toUpperCase(); })
            .join("-");
    }
    return jira_tag;
}
exports.getJiraTag = getJiraTag;
exports.main_branches = ["master", "develop", "devel", "main"];
exports.branch_types = ["main", "release", "feature", "user", "other"];
function getBranchType(slug_parts) {
    var type = slug_parts[0];
    if (exports.main_branches.includes(type)) {
        type = "main";
    }
    else if (!exports.branch_types.includes(type)) {
        type = "other";
    }
    return type;
}
exports.getBranchType = getBranchType;
function sameBranch(branch1, branch2) {
    var name1 = typeof branch1 === "string" ? branch1 : branch1.name;
    var name2 = typeof branch2 === "string" ? branch2 : branch2.name;
    return name1 === name2;
}
exports.sameBranch = sameBranch;
/**
 * Slug implementation taken from: https://github.com/rlespinasse/github-slug-action/blob/v3.x/src/slug.ts
 */
var MAX_SLUG_STRING_SIZE = 63;
/**
 * slug_cs will take envVar and then :
 * - replace any character by `-` except `0-9`, `a-z`, `.`, and `_`
 * - remove leading and trailing `-` character
 * - limit the string size to 63 characters
 * @param envVar to be slugged
 */
function slug_cs(envVar) {
    return trailHyphen(replaceAnyNonAlphanumericCharacter(envVar)).substring(0, MAX_SLUG_STRING_SIZE);
}
/**
 * slug will take envVar and then :
 * - put the variable content in lower case
 * - replace any character by `-` except `0-9`, `a-z`, `.`, and `_`
 * - remove leading and trailing `-` character
 * - limit the string size to 63 characters
 * @param envVar to be slugged
 */
function slug(envVar) {
    return slug_cs(envVar.toLowerCase());
}
exports.slug = slug;
function trailHyphen(envVar) {
    return envVar.replace(RegExp("^-*", "g"), "").replace(RegExp("-*$", "g"), "");
}
function replaceAnyNonAlphanumericCharacter(envVar) {
    return envVar.replace(RegExp("[^a-zA-Z0-9._]", "g"), "-");
}
function extractCommitData(commit_title) {
    var commit_title_parts = commit_title.split(" ");
    var actor = commit_title_parts.shift();
    var repository = commit_title_parts.shift();
    var project_name = commit_title_parts.length === 2 ? repository : commit_title_parts.shift();
    var action = commit_title_parts.shift();
    var branch_name = commit_title_parts.shift();
    return {
        actor: actor,
        action: action,
        project_name: project_name,
        repository: repository,
        branch_name: branch_name
    };
}
exports.extractCommitData = extractCommitData;
