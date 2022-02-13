import "/test/components/helper/context/consumer.sly";

describe("context handling", () => {
  let container;

  beforeAll(() => {
    class SomeParentElement extends HTMLElement {
      constructor() {
        super();
        this.callbacks = [];
        this._value = "foo";
      }
      _context(callback) {
        this.callbacks.push(callback);
        return {
          disconnect: () => {
            this.callbacks = this.callbacks.filter(
              (filterCallback) => filterCallback != callback
            );
          },
          value: this._value,
        };
      }
      set value(value) {
        this.callbacks.forEach((callback) => callback(value));
      }
    }

    customElements.define("some-parent-element", SomeParentElement);
  });

  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  describe("consumer", () => {
    it("updating consumer", () => {
      const parent = document.createElement("some-parent-element");
      container.appendChild(parent);

      const element = document.createElement(
        "test-components-helper-context-consumer"
      );
      element.foo = true;
      parent.appendChild(element);
      const consumerText = element.shadowRoot.childNodes[0].childNodes[0];

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
      expect(element.shadowRoot.childNodes[0].textContent).toBe("foo");
      expect(consumerText.textContent).toBe("foo");

      parent.value = "bar";

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
      expect(element.shadowRoot.childNodes[0].textContent).toBe("bar");
      expect(consumerText.textContent).toBe("bar");

      element.foo = false;

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
      expect(element.shadowRoot.childNodes[0].textContent).toBe("mep");

      parent.value = "baz";

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
      expect(element.shadowRoot.childNodes[0].textContent).toBe("mep");
      expect(consumerText.textContent).not.toBe("baz");
    });

    xit("consumer without fitting parent should throw exception", () => {
      const element = document.createElement(
        "test-components-helper-context-consumer"
      );
      // @TODO the Exception is thrown, but is caughtable, figure out how to test it anyway
      expect(() => container.appendChild(element)).toThrow(
        new Error('Could not find provider "some-parent-element"')
      );
    });
  });
});
