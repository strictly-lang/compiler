describe("record handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    })
    afterEach(() => {
        container.remove();
    });

    it("basic creation and update", () => {
        const element = document.createElement("test-components-structural-record-base");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].textContent).toBe("1 0");

        element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].textContent).toBe("3 0");
    });

    it("basic creation and update with destructuring", () => {
        const element = document.createElement("test-components-structural-record-destructure");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].textContent).toBe("1 0");

        element.shadowRoot.childNodes[0].dispatchEvent(new Event("click"));

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("BUTTON");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].textContent).toBe("3 0");
    });
});