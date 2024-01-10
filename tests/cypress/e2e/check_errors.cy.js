describe('app', () => {
  beforeEach(() => {
    cy.visit('http://127.0.0.1:3333/');
    cy.waitForStabilityAndCatchError("body");
  });

  it('Starts', () => {});

  it('Navigates to all tabs without error', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .each(($el) => {
        cy.wrap($el).as('tealTab');

        cy.get('@tealTab').then(($el2) => {
          cy.log(`Navigating to: ${$el2[0].innerText}`);
        });

        cy.get('@tealTab').click();
        cy.waitForStabilityAndCatchError("body");

        cy.get('@tealTab').invoke('attr', 'href').as('hrefTab');


        cy
          .get('@hrefTab')
          .then((hrefTab) => {
            cy
              .get(`${hrefTab}.tab-pane.active`)
              .should('be.visible')
              .within(() => {
                cy
                  .get('*')
                  .filter(':visible')
                  .should('have.length.gte', 1)
                  .then(($el3) => {
                    cy.wrap($el3).contains(/.+/);
                  });
              });
          });
      });
  });
});
