/**
 * This web app is meant for my school to utilize Chromebook Daily Loaners for students who forgot theirs at home or are waiting for theirs to be repaired. 
 * I developed this in collaboration with my school's Technology Coach. Our initial protocol was to utilize an analog sign-in/sign-out sheet. which worked for us for a while.
 *  The major drawbacks I identified to the analog system were: papers could be lost from the binder thus making record keeping inaccurate, Daily Loaners that were not returned from 30 days or more would become lost in the multitude of pages,
 * there was significant user input errors and lack of readibility with entries done by hand. I also could not easily identify trends such as how often specific students were borrowing loaners, what time of the month or during specific school events were
 * daily loaner check outs higher than average? 
 * The web app ensured that we had a better record management of Daily Loaners, it eliminated user input error in regards to the Chromebook's asset tag, because the web app searches through a current inventory Google Sheet that is up to date with all 
 * Daily Loaner asset tags and ensures a Chromebook being entered is entered correctly otherwise an error message will occur. Also it sends automated emails to both Technology Coordinators each morning at 8AM with the list of students and chromebooks that have not been returned
 * in 12+ hours. We can utilize these emails to work with School Counselors and Culture and Climate staff to help retrieve them or issue disciplinary actions if warranted. 
 * 
 * This web app has gone through the following iterations with logs:
 * V1- Inital implementation  Error: Did not write input to spreadsheet
 * V2- Spreadsheet ID assigned to push user input to the correct spreadsheet
 * V3- Director explained that input will get slower as more entries are added. It now creates a new spreadsheet for the month and year it is being signed out to reduce clutter and ease of organization
 * V4- Added Logo to HTML File
 * V5- Added error message created when sign-in does not match any known signed-out asset tags
 * V6- Assigned INVENTORY_SPREADSHEET_ID and INVENTORY_SHEET_NAME to have Sign-Out Asset Tags match current Daily loaner inventory following an incorrect loaner asset tag being entered
 * V7- Removed the requirement for Name and ID for Sign-In. Expedited sign-in process because it was taking extremely long for TCs to sign-in dropped off Chromebooks at end of day. 
 * V8- Added script to clear all text fields once a successful sign-out or sign-in was made
 * V9- Added script to HTML file to make ID and Asset Tag fields numbers only and will not accept text input. Will produce error message
 */ 
 
var SPREADSHEET_ID = "Spreadsheet_ID" // Replace with your Sign-Out log spreadsheet ID
var INVENTORY_SPREADSHEET_ID = "Inventory_Spreadhseet_ID"; // Replace with your Loaner Inventory Spreadsheet ID
var INVENTORY_SHEET_NAME = "Daily Loaners"; // Sheet name containing the list of valid Asset Tags
var EMAIL_RECIPIENTS = ["your.email@example.com", "another.email@example.com"]; // Replace with actual emails

/**
 * Handles HTTP GET requests when the web app is accessed.
 * Loads the web app UI from "Index.html".
 * @returns {HtmlOutput} The rendered HTML page.
 */
function doGet() {
  return HtmlService.createHtmlOutputFromFile('Index')
    .setTitle('MP Daily Loaner Sign In/ Sign Out');
}

/**
 * Retrieves the active Google Sheet for the current month.
 * If the sheet does not exist, it creates a new one with headers.
 * @returns {GoogleAppsScript.Spreadsheet.Sheet} The current month's sheet.
 */
function getMonthlySheet() {
  var ss = SpreadsheetApp.openById(SPREADSHEET_ID);
  var date = new Date();
  var monthYear = date.toLocaleString("en-US", { month: "long", year: "numeric" }); // Example: "March 2025"
  var sheet = ss.getSheetByName(monthYear);

  if (!sheet) {
    Logger.log("Creating new sheet: " + monthYear);
    sheet = ss.insertSheet(monthYear);
    sheet.appendRow(["Name", "ID", "Asset Tag", "Sign Out Date & Time", "Sign In Date & Time"]); // Column headers
  }

  return sheet;
}

/**
 * Checks if an Asset Tag exists in the "Daily Loaners" inventory sheet.
 * @param {string} assetTag - The Asset Tag to check.
 * @returns {boolean} - Returns true if the Asset Tag is found, otherwise false.
 */
function isValidAssetTag(assetTag) {
  var inventorySheet = SpreadsheetApp.openById(INVENTORY_SPREADSHEET_ID).getSheetByName(INVENTORY_SHEET_NAME);
  if (!inventorySheet) {
    Logger.log("ERROR: Inventory sheet not found.");
    return false;
  }

  var data = inventorySheet.getDataRange().getValues();
  for (var i = 1; i < data.length; i++) { // Skip header row
    if (data[i][0] == assetTag) {
      return true; // Asset Tag exists in inventory
    }
  }

  return false;
}

/**
 * Validates that ID and Asset Tag are 6-digit numbers.
 * @param {string} id - The inputted ID.
 * @param {string} assetTag - The inputted Asset Tag.
 * @returns {boolean|string} - Returns true if valid, otherwise an error message.
 */
function validateInput(id, assetTag) {
  var sixDigitPattern = /^\d{6}$/; // Regex: Must be exactly 6 digits (0-9)

  if (!sixDigitPattern.test(id) || !sixDigitPattern.test(assetTag)) {
    return "Asset Tag and ID must be a 6-digit number (i.e., 123456). Please check your input and try again.";
  }

  return true;
}

/**
 * Records a sign-out event if the Asset Tag exists in inventory.
 * @param {string} name - Borrower's name.
 * @param {string} id - 6-digit borrower ID.
 * @param {string} assetTag - Asset tag of the device.
 * @returns {string} - Confirmation message or error.
 */
function recordSignOut(name, id, assetTag) {
  var validation = validateInput(id, assetTag);
  if (validation !== true) {
    Logger.log("Validation Error: " + validation);
    return validation;
  }

  if (!isValidAssetTag(assetTag)) {
    Logger.log("ERROR: Asset Tag not found in inventory: " + assetTag);
    return "No Such Loaner in Inventory. Please check Asset Tag and try again.";
  }

  var sheet = getMonthlySheet();
  var dateTime = new Date().toLocaleString("en-US", { timeZone: "America/New_York" });

  if (!name || !id || !assetTag) {
    Logger.log("ERROR: Missing required fields.");
    return "Error: Please fill in all fields.";
  }

  sheet.appendRow([name, id, assetTag, dateTime, ""]);
  Logger.log("Sign-out recorded -> Name: " + name + ", ID: " + id + ", Asset Tag: " + assetTag + ", Time: " + dateTime);
  
  return "Sign-Out Successful!";
}

/**
 * Records a sign-in event by updating the corresponding row based on Asset Tag.
 * @param {string} assetTag - Asset Tag of the device being returned.
 * @returns {string} - Confirmation message or error.
 */
function recordSignIn(assetTag) {
  var sheet = getMonthlySheet();
  var data = sheet.getDataRange().getValues();
  var signInTime = new Date().toLocaleString("en-US", { timeZone: "America/New_York" });

  if (!assetTag) {
    Logger.log("ERROR: Missing Asset Tag for sign-in.");
    return "Error: Please enter an Asset Tag.";
  }

  for (var i = data.length - 1; i >= 1; i--) {
    if (data[i][2] == assetTag && data[i][4] === "") { // Find first open sign-out entry
      sheet.getRange(i + 1, 5).setValue(signInTime);
      Logger.log("Sign-in recorded -> Asset Tag: " + assetTag + ", Time: " + signInTime);
      return "Sign-In Successful!";
    }
  }

  Logger.log("WARNING: No matching sign-out found for Asset Tag: " + assetTag);
  return "Error: No record of this Asset Tag being signed out.";
}

/**
 * Runs daily to check overdue sign-outs (12+ hours overdue) and sends a single report.
 */
function checkOverdueLoaners() {
  var ss = SpreadsheetApp.openById(SPREADSHEET_ID);
  var sheets = ss.getSheets();
  var now = new Date();
  var overdueEntries = [];

  Logger.log("Running daily overdue check...");

  for (var s = 0; s < sheets.length; s++) {
    var sheet = sheets[s];
    var data = sheet.getDataRange().getValues();

    for (var i = 1; i < data.length; i++) {
      if (data[i][4] === "" && data[i][3]) { // If not signed in
        var signedOutTime = new Date(data[i][3]);
        var hoursDiff = (now - signedOutTime) / (1000 * 60 * 60);

        if (hoursDiff > 12) {
          overdueEntries.push(
            `Name: ${data[i][0]}\nID: ${data[i][1]}\nAsset Tag: ${data[i][2]}\nSigned Out: ${data[i][3]}\n`
          );
        }
      }
    }
  }

  if (overdueEntries.length > 0) {
    var message = `🚨 **Overdue Loaner Report** 🚨\n\nThe following devices are overdue (signed out for more than 12 hours):\n\n`;
    message += overdueEntries.join("\n----------------------\n");

    MailApp.sendEmail({
      to: EMAIL_RECIPIENTS.join(","),
      subject: "Daily Overdue Loaner Report",
      body: message
    });

    Logger.log("Daily overdue email sent with " + overdueEntries.length + " overdue records.");
  } else {
    Logger.log("No overdue loaners found.");
  }

  Logger.log("Daily overdue check completed.");
}
