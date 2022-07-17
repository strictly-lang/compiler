import util from "util";
import childProcess from "child_process";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const exec = util.promisify(childProcess.exec);

export default {
  files: [
    "test/integration/text.js",
    "test/integration/host/*.js",
    "test/integration/helper/if.js",
    // "test/integration/helper/each.js",
  ],
  nodeResolve: true,
  plugins: [
    {
      name: "strictly-plugin",
      resolve: {
        input: [".sly"],
        output: [".js"],
      },
      transform: async (context) => {
        const parsedPath = path.parse(context.request.url);
        if (parsedPath.ext === ".sly") {
          const { stdout, stderror } = await exec(
            `cabal run --verbose=silent strictly ${path.join(
              __dirname,
              context.request.url
            )}`
          );

          if (stderror) {
            throw stderror;
          }

          return {
            body: stdout,
            headers: {
              "Content-Type": "application/javascript; charset=utf-8",
            },
          };
        }
      },
    },
  ],
};
