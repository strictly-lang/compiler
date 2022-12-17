import { expect } from "@esm-bundle/chai";
import "/test/components/helper/if/base.sly";
import "/test/components/helper/if/remove.sly";

describe("if condition handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("condition should render the correct result, true first", () => {
    const element = document.createElement("test-components-helper-if-base");
    element.foo = true;
    element.bar = "baz";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-baz");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = false;

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-baz"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "barbar";

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-barbar"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = true;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-barbar");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "baz";

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-baz");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });

  it("condition should render the correct result, false first", () => {
    const element = document.createElement("test-components-helper-if-base");
    element.foo = false;
    element.bar = "baz";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-baz"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = true;

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-baz");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "barbar";

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-barbar");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = false;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-barbar"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "baz";

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-baz"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });

  it("condition should render the correct update correct when both times condition is positive", () => {
    const element = document.createElement("test-components-helper-if-base");
    element.foo = true;
    element.bar = "baz";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-baz");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
    let conditionContent = element.shadowRoot.childNodes[1];

    element.foo = true;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-baz");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
    expect(element.shadowRoot.childNodes[1]).to.equal(conditionContent);
  });

  it("condition should render the correct update correct when both times condition is negative", () => {
    const element = document.createElement("test-components-helper-if-base");
    element.foo = false;
    element.bar = "baz";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-baz"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
    let conditionContent = element.shadowRoot.childNodes[1];

    element.foo = false;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "not-foo-baz"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
    expect(element.shadowRoot.childNodes[1]).to.equal(conditionContent);
  });

  it("remove if", () => {
    const element = document.createElement("test-components-helper-if-remove");
    element.foo = true;
    element.bar = true;
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
