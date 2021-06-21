async function nextTick(amount) {
  for (let i = amount; i > 0; i--) {
    await new Promise((resolve) => resolve());
  }
}

describe("model element handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("basic model handling", () => {
    const element = document.createElement(
      "test-components-helper-model-counter"
    );
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("0");

    element.shadowRoot.childNodes[1].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1");

    element.shadowRoot.childNodes[1].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("2");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).toBe("1");
  });

  it("async model handling", async () => {
    const fetchSpy = spyOn(window, "fetch").and.callFake(function () {
      return new Promise((resolve) => {
        resolve({
          text: () => Promise.resolve("text response"),
        });
      });
    });

    const element = document.createElement(
      "test-components-helper-model-fetch"
    );
    element.load = true;
    element.id = 23;

    container.appendChild(element);

    expect(fetchSpy).toHaveBeenCalledOnceWith("/api/23");
    expect(element.shadowRoot.textContent).toBe("Loading...");

    await nextTick(10);

    expect(element.shadowRoot.textContent).toBe("text response");
  });
});
