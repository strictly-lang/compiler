import { expect } from "@esm-bundle/chai";
import "/test/components/host/base.sly";
import "/test/components/host/siblings.sly";
import "/test/components/host/nested.sly";
import "/test/components/host/attributes.sly";
import "/test/components/host/events.sly";
// import "/test/components/host/checkbox.sly";

describe("host element handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("basic creation", () => {
    const element = document.createElement("test-components-host-base");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
  });

  it("sibling element creation", () => {
    const element = document.createElement("test-components-host-siblings");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
  });

  it("nested element creation", () => {
    const element = document.createElement("test-components-host-nested");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(2);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).to.equal(
      "IMG"
    );
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("SPAN");
  });

  it("attribute test", () => {
    const element = document.createElement("test-components-host-attributes");
    element.bar = "baz";
    element.foo = "fooo";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).to.equal(1);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[0].className).to.equal("foo");
    expect(element.shadowRoot.childNodes[0].part.value).to.equal("baz");
    expect(element.shadowRoot.childNodes[0].title).to.equal(
      "combined fooo text"
    );

    element.bar = "barbar";

    expect(element.shadowRoot.childNodes[0].part.value).to.equal("barbar");
    expect(element.shadowRoot.childNodes[0].title).to.equal(
      "combined fooo text"
    );

    element.foo = "foofoo";
    expect(element.shadowRoot.childNodes[0].title).to.equal(
      "combined foofoo text"
    );
  });

  describe("input", () => {
    it("text change", () => {
      const element = document.createElement("test-components-host-events");
      element.value = "foo";
      element.oninput = (value) => {
        element.value = value;
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].tagName).to.equal("INPUT");
      expect(element.shadowRoot.childNodes[0].type).to.equal("text");
      expect(element.shadowRoot.childNodes[0].value).to.equal("foo");

      element.shadowRoot.childNodes[0].value = "fooa";
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].value).to.equal("fooa");
    });

    it("text non-change", () => {
      const element = document.createElement("test-components-host-events");
      element.value = "foo";
      element.oninput = (_evt) => {
        // Doing nothing with the event
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].tagName).to.equal("INPUT");
      expect(element.shadowRoot.childNodes[0].type).to.equal("text");
      expect(element.shadowRoot.childNodes[0].value).to.equal("foo");

      element.shadowRoot.childNodes[0].value = "fooa";
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].value).to.equal("foo");
    });

    it("text different-change", () => {
      const element = document.createElement("test-components-host-events");
      element.value = "foo";
      element.oninput = (_evt) => {
        element.value = "foob";
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].tagName).to.equal("INPUT");
      expect(element.shadowRoot.childNodes[0].type).to.equal("text");
      expect(element.shadowRoot.childNodes[0].value).to.equal("foo");

      element.shadowRoot.childNodes[0].value = "fooa";
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].value).to.equal("foob");
    });

    xit("checkbox change", () => {
      const element = document.createElement("test-components-host-checkbox");
      element.value = true;
      element.oninput = (value) => {
        element.value = value;
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].tagName).to.equal("INPUT");
      expect(element.shadowRoot.childNodes[0].type).to.equal("checkbox");
      expect(element.shadowRoot.childNodes[0].checked).to.equal(true);

      element.shadowRoot.childNodes[0].checked = false;
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].checked).to.equal(false);
    });

    xit("checkbox non-change", () => {
      const element = document.createElement("test-components-host-checkbox");
      element.value = true;
      element.oninput = (_evt) => {
        // Doing nothing with the event
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].tagName).to.equal("INPUT");
      expect(element.shadowRoot.childNodes[0].type).to.equal("checkbox");
      expect(element.shadowRoot.childNodes[0].checked).to.equal(true);

      element.shadowRoot.childNodes[0].checked = false;
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).to.equal(1);
      expect(element.shadowRoot.childNodes[0].checked).to.equal(true);
    });
  });
});
