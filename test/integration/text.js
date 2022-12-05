import { expect } from "@esm-bundle/chai";
import "/test/components/text/base.sly";
import "/test/components/text/dynamic.sly";
// import "/test/components/text/multidynamic.sly";
// import "/test/components/text/whitespace.sly";

describe("text element handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("basic text element creation", () => {
    const element = document.createElement("test-components-text-base");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("foo");
  });

  xit("dynamic text", () => {
    const element = document.createElement("test-components-text-dynamic");
    element.foo = "bar";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "con-bar-cat"
    );

    element.foo = "baz";

    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "con-baz-cat"
    );
  });

  xit("dynamic text with number", () => {
    const element = document.createElement("test-components-text-dynamic");
    element.foo = 1;
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal("con-1-cat");

    element.foo = "baz";

    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "con-baz-cat"
    );
  });

  xit("multi dynamic text", () => {
    const element = document.createElement("test-components-text-multidynamic");
    element.foo = "foo";
    element.bar = "bar";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "foo-bar-foo"
    );

    element.foo = "baz";

    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      "baz-bar-baz"
    );
  });

  xit("whitespace", () => {
    const element = document.createElement("test-components-text-whitespace");
    element.bar = "bar";
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      " foo  bar  baz "
    );

    element.bar = "barbar";

    expect(element.shadowRoot.childNodes[0].textContent).to.equal(
      " foo  barbar  baz "
    );
  });
});
