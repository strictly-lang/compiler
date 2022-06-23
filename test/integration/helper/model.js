import { expect } from "@esm-bundle/chai";
import "/test/components/helper/model/counter.sly";
import "/test/components/helper/model/fetch.sly";

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

    expect(element.shadowRoot.childNodes.length).to.equal(3);
    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("0");

    element.shadowRoot.childNodes[1].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("1");

    element.shadowRoot.childNodes[1].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("2");

    element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

    expect(element.shadowRoot.childNodes[0].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[1].tagName).to.equal("BUTTON");
    expect(element.shadowRoot.childNodes[2].textContent).to.equal("1");
  });

  it("async model handling", async () => {
    const originalFetch = window.fetch;
    window.fetch = (requestInfo) =>
      new Promise((resolve) => {
        resolve({
          text: () => Promise.resolve("text response " + requestInfo),
        });
      });

    const element = document.createElement(
      "test-components-helper-model-fetch"
    );
    element.load = true;
    element.id = 23;

    container.appendChild(element);

    expect(element.shadowRoot.textContent).to.equal("Loading...");

    await nextTick(10);

    expect(element.shadowRoot.textContent).to.equal("text response /api/23");

    element.id = 5;

    expect(element.shadowRoot.textContent).to.equal("Loading...");

    await nextTick(10);

    expect(element.shadowRoot.textContent).to.equal("text response /api/5");

    window.fetch = originalFetch;
  });
});
