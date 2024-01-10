Cypress.Commands.add(
  "waitForStabilityAndCatchError",
  (selector, stabilityPeriod = 300) => {
    let lastInnerHTML = "";
    let timesRun = 0;
    const checkInterval = 100;
    const maxTimesRun = stabilityPeriod / checkInterval;

    function checkForChanges() {
      cy.get(selector).then(($el) => {
        // Check for shiny-output-error class anywhere in the body
        if (Cypress.$("body").find(".shiny-output-error").length > 0) {
          throw new Error(
            "shiny-output-error class detected during stability check"
          );
        }

        const currentInnerHTML = $el.prop("innerHTML");
        if (currentInnerHTML !== lastInnerHTML) {
          lastInnerHTML = currentInnerHTML;
          timesRun = 0;
        } else if (timesRun < maxTimesRun) {
          timesRun += 1;
        } else {
          return;
        }
        cy.wait(checkInterval).then(checkForChanges);
      });
    }

    checkForChanges();
  }
);
