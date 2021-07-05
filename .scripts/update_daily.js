"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs_1 = require("fs");
var child_process_1 = require("child_process");
var helpers_1 = require("./helpers");
var view_state = JSON.parse(fs_1.readFileSync("./tmp/www/view_state.json", "utf-8"));
var _a = helpers_1.extractCommitData(process.argv.filter(function (arg) { return arg !== "--experimental-modules"; })[2]), action = _a.action, repository = _a.repository, project_name = _a.project_name, branch_name = _a.branch_name;
var project = helpers_1.getProject(project_name, repository, view_state.PROJECTS);
// If the model changed, we need to update the files.
if (helpers_1.actionHandler(action, project, branch_name)) {
    var index_html = fs_1.readFileSync("./tmp/index.html", "utf8");
    // Generate some id that we can use to cache bust the file with. Date should be sufficient.
    var id = Date.now();
    if (index_html.includes(helpers_1.script_tag(view_state.ID))) {
        index_html = index_html.replace(helpers_1.script_tag(view_state.ID), helpers_1.script_tag(id));
    }
    else {
        index_html = index_html.replace('<div id="daily_site_model"></div>', helpers_1.script_tag(id));
    }
    // Update the id with new date value.
    view_state.ID = id;
    var model = "\n    // We wrap in a JSON.parse to tell the js engine that it's just a json, which is faster to parse then an ordinary js object.\n    // https://v8.dev/blog/cost-of-javascript-2019#json.\n    window.AXIOM_DAILY_SITE_DATA = JSON.parse('" + JSON.stringify(view_state) + "');\n    // Prevent mutation.\n    Object.freeze(window.AXIOM_DAILY_SITE_DATA);";
    // Delete old view model.
    try {
        child_process_1.execSync("find www -maxdepth 1 -type f -name 'view_model.*.js' -delete");
    }
    catch (e) {
        console.log(e);
    }
    // Save all updated files to the repository.
    fs_1.writeFileSync("./tmp/www/view_state.json", JSON.stringify(view_state, null, 4));
    fs_1.writeFileSync("./tmp/www/view_model." + id + ".js", model);
    fs_1.writeFileSync("./tmp/index.html", index_html);
}
