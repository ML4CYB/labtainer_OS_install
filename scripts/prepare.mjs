import os from "os";
import process from "child_process";
import fs from "fs";
import path from "path";

async function commandExists(command) {
  try {
    if (os.platform() == "windows") {
      process.exec(`where ${command}`);
    } else {
      process.exec(`command -v ${command}`);
    }
  } catch (error) {
    return false;
  }
  return true;
}

async function main() {
  if (!(await commandExists("pre-commit"))) {
    console.log("pre-commit is not installed, see contributing.md'");
  }

  if (!fs.existsSync(path.join(".git", "hooks", "pre-commit"))) {
    const stdout = process.execSync("pre-commit install", { encoding: "utf8" });
    console.log(stdout);
  }
}

main();
