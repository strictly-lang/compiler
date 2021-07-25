import "/test/components/helper/context/consumer.sly";

describe("context handling", () => {
  let container;

  beforeAll(() => {
    class SomeParentElement extends HTMLElement {
      constructor() {
        super();
        this.value = "foo";
        this.callbacks = [];
      }
      _context(callback) {
        this.callbacks.push(callback);
        return {
          disconnect: () => {
            this.callbacks = this.callbacks.filter(
              (filterCallback) => filterCallback != callback
            );
          },
          value: this.value,
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
      parent.appendChild(element);

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
      expect(element.shadowRoot.childNodes[0].textContent).toBe("foo");

      parent.value = "bar";

      expect(element.shadowRoot.childNodes.length).toBe(1);
      expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
      expect(element.shadowRoot.childNodes[0].textContent).toBe("bar");
    });

    it("consumer without fitting parent should throw exception", () => {
      const element = document.createElement(
        "test-components-helper-context-consumer"
      );
      expect(() => parent.appendChild(element)).toThrowError(
        'Could not find provider "some-parent-element"'
      );
    });
  });
});
