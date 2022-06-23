import { expect } from "@esm-bundle/chai";
import "/test/components/helper/match/base.sly";
import "/test/components/helper/match/siblings.sly";
import "/test/components/helper/match/nested.sly";
import "/test/components/helper/match/update.sly";
import "/test/components/helper/match/remove.sly";

describe("match case handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("with basic case", () => {
    const element = document.createElement("test-components-helper-match-base");

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("first");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes[0].textContent).to.equal("second: 1");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
  });

  it("with siblings case", () => {
    const element = document.createElement(
      "test-components-helper-match-siblings"
    );

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("first");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("second: 1");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("third: 3");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
  });

  it("with nested case", () => {
    const element = document.createElement(
      "test-components-helper-match-nested"
    );

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("first");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "second nestedValue: 3 siblingValue: 1"
    );
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes[0].textContent).to.equal("third: 6");
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
  });

  it("with updating", () => {
    const element = document.createElement(
      "test-components-helper-match-update"
    );

    element.foo = "bar";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).to.equal(
      "BUTTON"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[0].textContent).to.equal(
      "first: 1 bar"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[1].tagName).to.equal(
      "SPAN"
    );

    element.foo = "baz";

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).to.equal(
      "BUTTON"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[0].textContent).to.equal(
      "first: 1 baz"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[1].tagName).to.equal(
      "SPAN"
    );

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).to.equal(
      "BUTTON"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[0].textContent).to.equal(
      "second: 2 baz"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[1].tagName).to.equal(
      "SPAN"
    );

    element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(
      new MouseEvent("click")
    );

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).to.equal(
      "BUTTON"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[0].textContent).to.equal(
      "second: 8 baz"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[1].tagName).to.equal(
      "SPAN"
    );

    element.foo = "mep";

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).to.equal(
      "BUTTON"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[0].textContent).to.equal(
      "second: 8 mep"
    );
    expect(element.shadowRoot.childNodes[0].childNodes[1].tagName).to.equal(
      "SPAN"
    );
  });

  it("with removing", () => {
    const element = document.createElement(
      "test-components-helper-match-remove"
    );
    element.foo = true;

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = false;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SECTION");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = true;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });
});
