import { expect } from "@esm-bundle/chai";
import "/test/components/structural/record/base.sly";
import "/test/components/structural/record/destructure.sly";

describe("record handling", () => {
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
      "test-components-structural-record-base"
    );
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("1 0");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("3 0");
  });

  it("basic creation and update with destructuring", () => {
    const element = document.createElement(
      "test-components-structural-record-destructure"
    );
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("1 0");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("3 0");
  });
});
