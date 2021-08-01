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

    expect(element.shadowRoot.childNodes.length).toBe(4);
    expect(element.shadowRoot.childNodes[0].tagName).toBe("STYLE");
    expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[2].tagName).toBe("DIV");
    expect(element.shadowRoot.childNodes[3].tagName).toBe("DIV");

    const styleOfPlainDiv = window.getComputedStyle(
      element.shadowRoot.childNodes[1]
    );
    const styleOfDivWithIdAndClass = window.getComputedStyle(
      element.shadowRoot.childNodes[2]
    );
    const styleOfDivWithClass = window.getComputedStyle(
      element.shadowRoot.childNodes[3]
    );

    debugger;
    expect(styleOfPlainDiv.paddingTop).toBe("1px");
    expect(styleOfPlainDiv.paddingRight).toBe("1px");
    expect(styleOfPlainDiv.paddingBottom).toBe("1px");
    expect(styleOfPlainDiv.paddingRight).toBe("1px");
    expect(styleOfPlainDiv.backgroundImage).toBe("none");

    const baseFontSIze = 16;
    expect(styleOfDivWithIdAndClass.paddingTop).toBe(`${2 * baseFontSIze}px`);
    expect(styleOfDivWithIdAndClass.paddingRight).toBe(`${3 * baseFontSIze}px`);
    expect(styleOfDivWithIdAndClass.paddingBottom).toBe(
      `${4 * baseFontSIze}px`
    );
    expect(styleOfDivWithIdAndClass.paddingLeft).toBe(`${5 * baseFontSIze}px`);
    expect(styleOfDivWithIdAndClass.backgroundColor).toBe("rgba(0, 0, 0, 0)");

    expect(styleOfDivWithClass.paddingTop).toBe("1px");
    expect(styleOfDivWithClass.paddingRight).toBe("1px");
    expect(styleOfDivWithClass.paddingBottom).toBe("1px");
    expect(styleOfDivWithClass.paddingRight).toBe("1px");
    expect(styleOfDivWithClass.backgroundColor).toBe("rgb(100, 200, 255)");
  });
});
