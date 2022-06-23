import { expect } from "@esm-bundle/chai";
import "/test/components/text/dynamic.sly";

describe("host element handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("dynamic text should not interpret html, but just show it as a string", () => {
    const element = document.createElement("test-components-text-dynamic");
    element.foo = "<div >";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "con-<div >-cat"
    );
    expect(element.shadowRoot.childNodes[0].querySelector("div")).to.equal(
      null
    );
    element.foo = "<span >";

    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "con-<span >-cat"
    );
    expect(element.shadowRoot.childNodes[0].querySelector("span")).to.equal(
      null
    );
  });
});
