const fs = require('fs');
const path = require('path');

const directoryPath = path.join(__dirname, './'); // Starting directory
const chineseCharRegex = /[\u3400-\u9FBF]/; // Regex to match Chinese characters

function readFilesInDirectory(directory) {
    fs.readdir(directory, { withFileTypes: true }, (err, files) => {
        if (err) {
            return console.log('Unable to scan directory: ' + err);
        }

        files.forEach(file => {
            let fullPath = path.join(directory, file.name);
            if (file.isDirectory()) {
                readFilesInDirectory(fullPath); // Recursively read subdirectories
            } else {
                if(!fullPath.endsWith(".java")){
                    return;
                }
                // Reading file content
                fs.readFile(fullPath, 'utf8', (err, content) => {
                    if (err) {
                        return console.log(err);
                    }

                    // Splitting content into lines and checking for Chinese characters
                    const lines = content.split('\n');
                    const linesWithChinese = lines.filter(line => chineseCharRegex.test(line));

                    if (linesWithChinese.length > 0) {
                        console.log(`File Name: ${fullPath}`);
                        linesWithChinese.forEach((line, index) => {
                            console.log(`Line ${index + 1}: ${line}`);
                        });
                        console.log('----------------------');
                    }
                });
            }
        });
    });
}

readFilesInDirectory(directoryPath);
