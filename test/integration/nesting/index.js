import { expect } from "@esm-bundle/chai";
import "/test/components/nesting/absolute.sly";
import "/test/components/nesting/relative.sly";

describe("component handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("creating nested absolute component", () => {
    const element = document.createElement("test-components-nesting-absolute");
    element.foo = "bar";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal(
      "TEST-COMPONENTS-NESTING-DEEP-INDEX"
    );
    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes.length
    ).to.equal(1);
    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes[0].tagName
    ).to.equal("DIV");
    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes[0].textContent
    ).to.equal("nested component value: bar");

    element.foo = "baz";

    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes[0].textContent
    ).to.equal("nested component value: baz");
  });

  it("creating nested relative component", () => {
    const element = document.createElement("test-components-nesting-relative");
    element.foo = "bar";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal(
      "TEST-COMPONENTS-NESTING-DEEP-INDEX"
    );
    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes.length
    ).to.equal(1);
    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes[0].tagName
    ).to.equal("DIV");
    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes[0].textContent
    ).to.equal("nested component value: bar");

    element.foo = "baz";

    expect(
      element.shadowRoot.childNodes[0].shadowRoot.childNodes[0].textContent
    ).to.equal("nested component value: baz");
  });
});
