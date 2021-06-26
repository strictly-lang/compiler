import "/test/components/helper/each/base.js";

describe("each loop handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("with filled array", () => {
    const element = document.createElement("test-components-helper-each-base");
    element.foo = ["foo", "bar", "baz"];
    element.bar = "mep";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(5);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-mep");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1-bar-mep");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2-baz-mep");
    expect(element.shadowRoot.childNodes[4].tagName).toBe("FOOTER");

    element.foo = ["foo2", "bar2", "baz2"];

    expect(element.shadowRoot.childNodes.length).toBe(5);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo2-mep");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1-bar2-mep");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2-baz2-mep");
    expect(element.shadowRoot.childNodes[4].tagName).toBe("FOOTER");
  });

  it("with growing array", () => {
    const element = document.createElement("test-components-helper-each-base");
    element.foo = [];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.bar = "bob";

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("Empty list bob");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.foo = ["foo", "bar", "baz"];

    expect(element.shadowRoot.childNodes.length).toBe(5);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-bob");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1-bar-bob");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2-baz-bob");
    expect(element.shadowRoot.childNodes[4].tagName).toBe("FOOTER");

    element.bar = "alice";

    expect(element.shadowRoot.childNodes.length).toBe(5);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-alice");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1-bar-alice");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2-baz-alice");
    expect(element.shadowRoot.childNodes[4].tagName).toBe("FOOTER");
  });

  it("with shrinking array", () => {
    const element = document.createElement("test-components-helper-each-base");
    element.foo = ["foo", "bar", "baz"];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(5);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-alice");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1-bar-alice");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2-baz-alice");
    expect(element.shadowRoot.childNodes[4].tagName).toBe("FOOTER");

    element.foo = ["foo"];

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-alice");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.bar = "bob";

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-bob");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.foo = [];

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("Empty list bob");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.bar = "alice";

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");
  });

  it("with resetting empty array", () => {
    const element = document.createElement("test-components-helper-each-base");

    element.foo = [];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.foo = [];

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

    element.bar = "bob";

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("Empty list bob");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");
  });
});
