import { expect } from "@esm-bundle/chai";
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

    expect(element.shadowRoot.childNodes.length).to.equal(4);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("STYLE");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[2].tagName).to.equal("DIV");
    expect(element.shadowRoot.childNodes[3].tagName).to.equal("DIV");

    const styleOfPlainDiv = window.getComputedStyle(
      element.shadowRoot.childNodes[1]
    );
    const styleOfDivWithIdAndClass = window.getComputedStyle(
      element.shadowRoot.childNodes[2]
    );
    const styleOfDivWithClass = window.getComputedStyle(
      element.shadowRoot.childNodes[3]
    );

    expect(styleOfPlainDiv.paddingTop).to.equal("1px");
    expect(styleOfPlainDiv.paddingRight).to.equal("1px");
    expect(styleOfPlainDiv.paddingBottom).to.equal("1px");
    expect(styleOfPlainDiv.paddingRight).to.equal("1px");
    expect(styleOfPlainDiv.backgroundImage).to.equal("none");

    const baseFontSIze = 16;
    expect(styleOfDivWithIdAndClass.paddingTop).to.equal(
      `${2 * baseFontSIze}px`
    );
    expect(styleOfDivWithIdAndClass.paddingRight).to.equal(
      `${3 * baseFontSIze}px`
    );
    expect(styleOfDivWithIdAndClass.paddingBottom).to.equal(
      `${4 * baseFontSIze}px`
    );
    expect(styleOfDivWithIdAndClass.paddingLeft).to.equal(
      `${5 * baseFontSIze}px`
    );
    expect(styleOfDivWithIdAndClass.backgroundColor).to.equal(
      "rgba(0, 0, 0, 0)"
    );

    expect(styleOfDivWithClass.paddingTop).to.equal("1px");
    expect(styleOfDivWithClass.paddingRight).to.equal("1px");
    expect(styleOfDivWithClass.paddingBottom).to.equal("1px");
    expect(styleOfDivWithClass.paddingRight).to.equal("1px");
    expect(styleOfDivWithClass.backgroundColor).to.equal("rgb(100, 200, 255)");
  });
});
