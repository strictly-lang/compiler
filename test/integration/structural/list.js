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
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(4);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe(
      "first: 0, second: 1"
    );
    expect(element.shadowRoot.childNodes[2].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("0: 2");
    expect(element.shadowRoot.childNodes[3].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("1: 3");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes.length).toBe(4);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    expect(element.shadowRoot.childNodes[1].textContent).toBe(
      "first: 0, second: 2"
    );
    expect(element.shadowRoot.childNodes[2].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("0: 4");
    expect(element.shadowRoot.childNodes[3].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[3].textContent).toBe("1: 6");
  });
});
