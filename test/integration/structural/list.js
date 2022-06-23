import { expect } from "@esm-bundle/chai";
import "/test/components/structural/list/base.sly";
import "/test/components/structural/list/destructure.sly";
import "/test/components/structural/list/multisource.sly";

describe("list handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("basic creation and update", () => {
    const element = document.createElement(
      "test-components-structural-list-base"
    );
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(4);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0: 0");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("1: 1");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal("2: 3");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0: 1");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("1: 2");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal("2: 3");
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[4].textContent).to.equal("3: 6");
  });

  it("destructure list", () => {
    const element = document.createElement(
      "test-components-structural-list-destructure"
    );
    element.values = [];
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "empty values"
    );

    element.values = ["foo"];

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "one value foo"
    );

    element.values = ["foo", "bar"];

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "first: foo, second: bar"
    );
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("empty rest");

    element.values = ["foo1", "bar1", "baz"];

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "first: foo1, second: bar1"
    );
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0: baz");
  });

  it("multisource list", () => {
    const element = document.createElement(
      "test-components-structural-list-multisource"
    );
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(8);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");

    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0: 3");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("0: 4");

    expect(element.shadowRoot.childNodes[3].textContent).to.equal("1: 3");
    expect(element.shadowRoot.childNodes[4].textContent).to.equal("1: 4");

    expect(element.shadowRoot.childNodes[5].textContent).to.equal("2: 3");
    expect(element.shadowRoot.childNodes[6].textContent).to.equal("2: 4");

    expect(element.shadowRoot.childNodes[7].tagName).to.equal("FOOTER");
  });
});
