import "/test/components/structural/list/base.sly";
import "/test/components/structural/list/destructure.sly";
import "/test/components/structural/list/multisource.sly";

describe("list handling", () => {
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
      "test-components-structural-list-base"
    );

    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(4);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0: 0");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1: 1");
    expect(element.shadowRoot.childNodes[3].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2: 3");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes.length).toBe(5);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0: 1");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1: 2");
    expect(element.shadowRoot.childNodes[3].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("2: 3");
    expect(element.shadowRoot.childNodes[4].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[4].textContent).toBe("3: 6");
  });

  it("destructure list", () => {
    const element = document.createElement(
      "test-components-structural-list-destructure"
    );
    element.values = [];
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(1);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).toBe("empty values");

    element.values = ["foo"];

    expect(element.shadowRoot.childNodes.length).toBe(1);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).toBe("one value foo");

    element.values = ["foo", "bar"];

    expect(element.shadowRoot.childNodes.length).toBe(2);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).toBe(
      "first: foo, second: bar"
    );
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("empty rest");

    element.values = ["foo1", "bar1", "baz"];

    expect(element.shadowRoot.childNodes.length).toBe(2);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[0].textContent).toBe(
      "first: foo1, second: bar1"
    );
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].textContent).toBe("0: baz");
  });

  it("multisource list", () => {
    const element = document.createElement(
      "test-components-structural-list-multisource"
    );
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(8);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");

    expect(element.shadowRoot.childNodes[1].textContent).toBe("0: 3");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("0: 4");

    expect(element.shadowRoot.childNodes[3].textContent).toBe("1: 3");
    expect(element.shadowRoot.childNodes[4].textContent).toBe("1: 4");

    expect(element.shadowRoot.childNodes[5].textContent).toBe("2: 3");
    expect(element.shadowRoot.childNodes[6].textContent).toBe("2: 4");

    expect(element.shadowRoot.childNodes[7].tagName).toBe("FOOTER");
  });
});
