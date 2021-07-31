import "/test/components/style/base.sly";

describe("style handling", () => {
  let container;
  beforeEach(() => {
    container = document.createElement("div");
    document.body.appendChild(container);
  });
  afterEach(() => {
    container.remove();
  });

  it("basic creation", () => {
    const element = document.createElement("test-components-style-base");
    container.appendChild(element);

    expect(element.shadowRoot.childNodes.length).toBe(3);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("DIV");

    const styleOfPlainDiv = window.getComputedStyle(
      element.shadowRoot.childNodes[0]
    );
    const styleOfDivWithIdAndClass = window.getComputedStyle(
      element.shadowRoot.childNodes[1]
    );
    const styleOfDivWithClass = window.getComputedStyle(
      element.shadowRoot.childNodes[2]
    );

    expect(styleOfPlainDiv.padding).toBe("1px");
    expect(styleOfPlainDiv.backgroundImage).toBe("none");

    expect(styleOfDivWithIdAndClass.padding).toBe("2rem 3rem 4rem 5rem");
    expect(styleOfDivWithIdAndClass.backgroundImage).toBe("none");

    expect(styleOfDivWithClass.padding).toBe("1px");
    expect(styleOfDivWithClass.backgroundImage).toBe('url("/foo.png")');
  });
});
