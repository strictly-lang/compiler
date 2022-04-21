import "/test/components/host/base.sly";
import "/test/components/host/siblings.sly";
import "/test/components/host/nested.sly";
import "/test/components/host/attributes.sly";
import "/test/components/host/events.sly";
import "/test/components/host/checkbox.sly";

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

    expect(element.shadowRoot.childNodes.length).toBe(1);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
  });

  it("sibling element creation", () => {
    const element = document.createElement("test-components-host-siblings");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(2);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
  });

  it("nested element creation", () => {
    const element = document.createElement("test-components-host-nested");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(2);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[0].childNodes.length).toBe(1);
    expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).toBe("IMG");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
  });

  it("attribute test", () => {
    const element = document.createElement("test-components-host-attributes");
    element.bar = "baz";
    element.foo = "fooo";

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(1);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[0].className).toBe("foo");
    expect(element.shadowRoot.childNodes[0].part.value).toBe("baz");
    expect(element.shadowRoot.childNodes[0].title).toBe("combined fooo text");

    element.bar = "barbar";

    expect(element.shadowRoot.childNodes[0].part.value).toBe("barbar");
    expect(element.shadowRoot.childNodes[0].title).toBe("combined fooo text");

    element.foo = "foofoo";
    expect(element.shadowRoot.childNodes[0].title).toBe("combined foofoo text");
  });

  xdescribe("input", () => {
    it("text change", () => {
      const element = document.createElement("test-components-host-events");
      element.value = "foo";
      element.oninput = (value) => {
        element.value = value;
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("INPUT");
      expect(element.shadowRoot.childNodes[0].type).toBe("text");
      expect(element.shadowRoot.childNodes[0].value).toBe("foo");

      element.shadowRoot.childNodes[0].value = "fooa";
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].value).toBe("fooa");
    });

    it("text non-change", () => {
      const element = document.createElement("test-components-host-events");
      element.value = "foo";
      element.oninput = (_evt) => {
        // Doing nothing with the event
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("INPUT");
      expect(element.shadowRoot.childNodes[0].type).toBe("text");
      expect(element.shadowRoot.childNodes[0].value).toBe("foo");

      element.shadowRoot.childNodes[0].value = "fooa";
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].value).toBe("foo");
    });

    it("text different-change", () => {
      const element = document.createElement("test-components-host-events");
      element.value = "foo";
      element.oninput = (_evt) => {
        element.value = "foob";
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("INPUT");
      expect(element.shadowRoot.childNodes[0].type).toBe("text");
      expect(element.shadowRoot.childNodes[0].value).toBe("foo");

      element.shadowRoot.childNodes[0].value = "fooa";
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].value).toBe("foob");
    });

    it("checkbox change", () => {
      const element = document.createElement("test-components-host-checkbox");
      element.value = true;
      element.oninput = (value) => {
        element.value = value;
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("INPUT");
      expect(element.shadowRoot.childNodes[0].type).toBe("checkbox");
      expect(element.shadowRoot.childNodes[0].checked).toBe(true);

      element.shadowRoot.childNodes[0].checked = false;
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].checked).toBe(false);
    });

    it("checkbox non-change", () => {
      const element = document.createElement("test-components-host-checkbox");
      element.value = true;
      element.oninput = (_evt) => {
        // Doing nothing with the event
      };

      container.appendChild(element);

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("INPUT");
      expect(element.shadowRoot.childNodes[0].type).toBe("checkbox");
      expect(element.shadowRoot.childNodes[0].checked).toBe(true);

      element.shadowRoot.childNodes[0].checked = false;
      element.shadowRoot.childNodes[0].dispatchEvent(new Event("input"));

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].checked).toBe(true);
    });
  });
});
