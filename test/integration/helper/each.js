import { expect } from "@esm-bundle/chai";
import "/test/components/helper/each/base.sly";
// import "/test/components/helper/each/index.sly";
// import "/test/components/helper/each/constraint.sly";
// import "/test/components/helper/each/neverconstraint.sly";

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
    element.baz = true;
    element.foo = ["foo", "bar", "baz"];
    element.bar = "mep";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo-mep");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("bar-mep");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal("baz-mep");
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("FOOTER");

    element.foo = ["foo2", "bar2", "baz2"];

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("foo2-mep");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("bar2-mep");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal("baz2-mep");
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("FOOTER");
  });

  xit("with growing array", () => {
    const element = document.createElement("test-components-helper-each-index");
    element.baz = true;
    element.foo = [];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "bob";

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list bob"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = ["foo", "bar", "baz"];

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0-foo-bob");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("1-bar-bob");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal("2-baz-bob");
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("FOOTER");

    element.bar = "alice";

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "0-foo-alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal(
      "1-bar-alice"
    );
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal(
      "2-baz-alice"
    );
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("FOOTER");
  });

  xit("with shrinking array", () => {
    const element = document.createElement("test-components-helper-each-index");
    element.baz = true;
    element.foo = ["foo", "bar", "baz"];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "0-foo-alice"
    );
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal(
      "1-bar-alice"
    );
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal(
      "2-baz-alice"
    );
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("FOOTER");

    element.foo = ["foo"];

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "0-foo-alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "bob";

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0-foo-bob");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = [];

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list bob"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "alice";

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });

  xit("with resetting empty array", () => {
    const element = document.createElement("test-components-helper-each-index");
    element.baz = true;
    element.foo = [];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.foo = [];

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.bar = "bob";

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list bob"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });

  xit("removing each in filled-case", () => {
    const element = document.createElement("test-components-helper-each-index");
    element.baz = true;
    element.foo = ["foo", "bar", "baz"];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(5);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "0-foo-alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal(
      "1-bar-alice"
    );
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).to.equal(
      "2-baz-alice"
    );
    expect(element.shadowRoot.childNodes[4].tagName).to.equal("FOOTER");

    element.baz = false;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SECTION");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });

  xit("removing each in empty-case", () => {
    const element = document.createElement("test-components-helper-each-index");
    element.baz = true;
    element.foo = [];
    element.bar = "alice";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal(
      "Empty list alice"
    );
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");

    element.baz = false;

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SECTION");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });

  xit("entity needs to be skipped when constraint is not matched", () => {
    const element = document.createElement(
      "test-components-helper-each-constraint"
    );

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(4);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("0");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("2");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("FOOTER");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes.length).to.equal(4);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("1");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("2");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("FOOTER");
  });

  xit("when constraints are never matched, else is rendered", () => {
    const element = document.createElement(
      "test-components-helper-each-neverconstraint"
    );

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("HEADER");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).to.equal("else");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("FOOTER");
  });
});
