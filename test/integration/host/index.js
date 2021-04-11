describe("host element handling", () => {
    let container;
    beforeEach(() => {
       container = document.createElement("div");
       document.body.appendChild(container); 
    })
    afterEach(() => {
        container.remove();
    });

    it("basic host element creation", () => {
        const element = document.createElement("test-components-host-base-index");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    });

    it("basic sibling element creation", () => {
        const element = document.createElement("test-components-host-siblings-index");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    });
});